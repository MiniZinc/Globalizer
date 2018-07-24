{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module GroupOutput where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM hiding (check)
import Control.Monad.Catch
import Control.Exception.Lifted hiding (try,catch)
import Control.Exception.Lens
import Control.Lens hiding (Context, universe)
import Control.Monad.State.Strict
import Control.Monad.Writer (execWriter, tell)
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as BSC
import Data.Data
import Data.Data.Lens
import Data.Foldable (toList)
import Data.Generics.Uniplate.Data
import qualified Data.IntMap as IM
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (mempty, mconcat)
import Data.Ord
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Word
import qualified Data.Sequence as Seq
import GHC.Conc.Sync
import Language.MiniZinc hiding (solve)
import Language.MiniZinc.Bindings (addAssignmentsToBindings)
import Language.MiniZinc.Resolve
import Language.MiniZinc.SolutionParser
import Safe hiding (at)
import System.Clock
import System.Exit
import System.IO
import System.IO.Temp
import System.Process hiding (env)
import System.Random
import System.Timeout
import Text.Printf

import System.IO.Unsafe

import Arguments
import Bindings
import Common
import EvalModel
import Loc
import Misc
import Statistics
import Types

import SimpleLog

import GlobalizerOptions as GOpts

data AbortException = AbortException
  deriving (Show, Typeable)
instance Exception AbortException

-- Find the variables in an expression.  Does NOT descend into the
-- indices of array accesses.
--
-- For example, the call
--
--   variables . parseExp $ "x[i,1]+3 = y-z"
--
-- will return a list of three expressions: the ArrayAccess "x[i,1]"
-- and the Idents "y" and "z".
variables :: Expression -> [Expression']
variables = toList . execWriter . descendBiM f
  where f e@(Ident {})        = tell (Seq.singleton e) >> return e
        f e | isArrayAccess e = tell (Seq.singleton e) >> return e
        f e                   = descendM f e
        isArrayAccess e' = fromMaybe False $ do
                            ArrayAccess a _ <- return $ e'
                            Ident _ <- return $ a ^. expRawExpression
                            return True

-- Are two variables "similar"?  That is, do they have the same main
-- identifier?
--
-- Similar:     "x" and "x"
-- Similar:     "x[1]" and "x[2]"
--
-- Not similar: "x" and "y"
-- Not similar: "x[1]" and "y[1]"
similarVariables :: Expression' -> Expression' -> Bool
similarVariables (Ident i1)         (Ident i2)         = i1 == i2
similarVariables (ArrayAccess i1 _) (ArrayAccess i2 _) = i1 == i2
similarVariables _                  _                  = False

-- Group variables into equivalence classes by their similarity.
groupVariables :: [Expression'] -> [[Expression']]
groupVariables = partitionBy similarVariables

-- Determine the identifier of an expression.  Partial function.
expIdentifier :: Expression' -> VarId
expIdentifier (Ident v)                 = v
expIdentifier (ArrayAccess (expRaw -> Ident v) _) = v
expIdentifier x = error $ "expIdentifier: can't figure out identifier of " ++ show x

-- Determine the identfier of a list of expressions.  All expressions
-- must have the same identifier.
groupIdentifier :: [Expression'] -> VarId
groupIdentifier []     = error "groupIdentifier: empty list"
groupIdentifier (e:es) = let (i:is) = map expIdentifier (e:es)
                         in assert (allEqual is) i

-- Are all elements of a list equal?  Returns true for empty or
-- singleton lists.
allEqual :: Eq a => [a] -> Bool
allEqual (x:xs) = all (==x) xs
allEqual _      = True

-- Given a model and an identifier, find the top-level VarDecl that
-- defines that identifier.
findVarDecl :: Model -> VarId -> Maybe (VarDecl)
findVarDecl m vid =
    listToMaybe [ vd | vd@VarDecl { _varDeclIdent = vid' } <- topLevelVarDecls m,
                       vid == vid' ]

replaceOutput :: Model -> Model
replaceOutput m =
    let otherItems = filter (not . isOutputI) (m ^. modelItems)
        varIdents = [ i | VarDeclI vd <- otherItems,
                          tiInst (vd ^. varDeclTypeInst) == Var,
                          let i = vd ^. varDeclIdent ]
        newOutput = OutputI $ makeExp $ ArrayLit [ makeExp $ Call "show" [makeExp $ Ident i] | i <- varIdents ] [(1,genericLength varIdents)]
   in m & modelItems .~ (otherItems ++ [newOutput])


replaceSearch :: GlobalizerOptions -> Model -> Model
replaceSearch opts m = modelItems %~ ((++ newSolveItems) . filter (not . isSolveI)) $ m
  where newSolveItems =
         -- We used to use "random" as the variable selection here,
         -- but sometimes it's terrible.  Instead we use a good
         -- variable selection, but keep the random value selection
         -- to encourage diversity in solutions.
         [ if GOpts.freeSearch opts
           then SolveI (Annotations []) Nothing SolveSatisfy
           else SolveI (Annotations [makeExp $ Call "int_default_search" [ makeExp $ Call "dom_w_deg" [], makeExp $ Call "indomain_random" []]]) Nothing SolveSatisfy
         , IncludeI "gecode.mzn" Nothing ]


buildOutput :: [(Replacement, Double)] -> String
buildOutput replacements =
          concatMap (\l -> l ++ "\n") $
            [ "OUTPUT" ]
            ++ (flip map replacements $ \(rep,scr) ->
                  concat [ printf "%.2f" scr
                         -- , " "
                         -- , show ctx
                         , " "
                         , prettyPrintify rep ])

processGroupModels :: String -> Bindings -> Maybe Expression -> [Model]
                      -> GlobalizerOptions -> Maybe String -> ChannelMap
                      -> SimpleLog.Handle -> StateT Statistics IO [(Replacement, Double)]
processGroupModels dataFilePath env maybeContext models opts consFilter channelMap logHandle = do
  let m = head models
  SimpleLog.logPrint logHandle LogDebug m
  SimpleLog.log logHandle LogDebug (plainShow m)
  SimpleLog.log logHandle LogDebug $ fromMaybe "<no context>" $ showExp2 <$> maybeContext

  -- Process all the models.
  --
  -- This is essentially a process of reducing the set of possible
  -- replacements.  We start with "Nothing", which indicates that we
  -- have no current set of candidate replacements.  Processing the
  -- first model will give a list of replacements.  We then pass this
  -- to the next call to processModel, which will return a subset of
  -- that list, and so on.  Once it is finished we have the
  -- intersection of the models' tolerated replacements.
  SimpleLog.log logHandle LogDebug "processing group"
  inter <- statisticsTime "processing groups" $ do
    SimpleLog.log logHandle LogDebug "starting model"
    result <- trying failedProcessReason $
                fromMaybe (error "empty group?") <$> foldM (\reps (modelNumber, m') -> statisticsTime (T.pack ("model " ++ show (modelNumber :: Int))) $ do
                                             SimpleLog.log logHandle LogDebug (plainShow m')
                                             void $ liftIO $ evaluate $ maybe 0 length reps
                                             Just <$> processModelWrapper dataFilePath env maybeContext m' (reps :: Maybe [(Replacement,Double)]) opts consFilter channelMap logHandle)
                                          Nothing
                                          (zip [1..] models)
    case result of
      Left NoSolutions -> return []
      Left TooBig -> return []
      Left ImpliesCheck -> statisticsSuccessfulImpliesCheck >> return []
      Right replacementList -> do
          let inter' = replacementList
          SimpleLog.log logHandle LogDebug "intersection is:"
          mapM_ (SimpleLog.logPrint logHandle LogDebug) inter'
          SimpleLog.log logHandle LogDebug "done"

          let inter = tightest (topLevelBindings m) inter'
          SimpleLog.log logHandle LogDebug "non-dominated intersection is:"
          mapM_ (SimpleLog.logPrint logHandle LogDebug) inter
          SimpleLog.log logHandle LogDebug "done"
          return inter
  return inter

timingHandle :: MVar (Maybe System.IO.Handle)
{-# NOINLINE timingHandle #-}
timingHandle = unsafePerformIO (newMVar Nothing)

getTimingHandle :: IO System.IO.Handle
getTimingHandle = do
  mh <- takeMVar timingHandle
  case mh of
    Nothing -> do
      h <- openFile "timing-output" WriteMode
      return h
    Just h -> return h

replaceTimingHandle :: System.IO.Handle -> IO ()
replaceTimingHandle h = do
  putMVar timingHandle (Just h)

-- Change this to enable/disable timing output.
doTiming :: Bool
doTiming = False

timeAction :: MonadIO m => String -> m a -> m a
timeAction label action =
  case doTiming of
    False -> action
    True -> do
      t1 <- liftIO (getTime Monotonic)
      x <- action
      t2 <- liftIO (getTime Monotonic)
      let nano = toNanoSecs t2 - toNanoSecs t1
      liftIO $ do
        h <- getTimingHandle
        hPutStrLn h $ "TIME " ++ label ++ " " ++ show nano
        hFlush h
        replaceTimingHandle h
      return x

timeScoreReplacement
  :: String
     -> Bindings
     -> Maybe (Expression)
     -> Model
     -> [Expression']
     -> Replacement
     -> GOpts.GlobalizerOptions
     -> SimpleLog.Handle
     -> StateT Statistics IO (Replacement, Double)
timeScoreReplacement dataFilePath env maybeContext m outVars c opts logHandle = do
  timeAction "scoreReplacement" (scoreReplacement dataFilePath env maybeContext m outVars c opts logHandle)

scoreReplacement
  :: String
     -> Bindings
     -> Maybe (Expression)
     -> Model
     -> [Expression']
     -> Replacement
     -> GOpts.GlobalizerOptions
     -> SimpleLog.Handle
     -> StateT Statistics IO (Replacement, Double)
scoreReplacement dataFilePath env maybeContext m outVars c opts logHandle = do
  let nSampleSols = GOpts.nSampleSolutions opts
  let newModel = Model {
               _modelItems =
                 map VarDeclI (topLevelVarDecls m)
                 ++ annotationItems m
                 ++ filter isFunctionI (m ^. modelItems)
                 ++ instantiate c
                 ++ maybe [] ((:[]) . ConstraintI) maybeContext
                 ++ [SolveI (Annotations [makeExp $ Call "int_default_search" [makeExp $ Call "dom_w_deg" [], makeExp $ Call "indomain_random" []]]) Nothing SolveSatisfy]
                 ++ [OutputI (makeExp $ ArrayLit (map (\v -> makeExp $ Call "show" [v]) (map makeExp $ outVars)) [(1,genericLength outVars)])]
                 ++ [IncludeI "glob.mzn" (Nothing)]
                 ++ [IncludeI "gecode.mzn" (Nothing)]
                 }
  let evalledNewModel = Language.MiniZinc.evalModelArraySlices newModel

  recordLogKey "new model" (plainShow evalledNewModel)
  when (True || name (fst c) == "sliding_sum") $ SimpleLog.log logHandle LogScoring $ "SOLVING FOR CONSTRAINT: " ++ prettyPrintify c
  (_, csols) <-
    timeAction "scoreReplacement/solve" $
      solve dataFilePath True evalledNewModel opts logHandle
  when (length csols < nSampleSols) $ do
    SimpleLog.log logHandle LogScoring $ "didn't find enough solutions to constraint (" ++ prettyPrintify c ++ ")"
    SimpleLog.log logHandle LogScoring $ "found only " ++ show (length csols) ++ " solutions using this model:"
    SimpleLog.log logHandle LogScoring $ plainShow evalledNewModel

  results <- 
   timeAction "solveReplacement/checkSols" $
   statisticsTime "forM csols" $ forM (zip [0::Int ..] csols) $ \(solnum, s) -> statisticsTime (T.pack ("solution " ++ show solnum)) $ do
    SimpleLog.log logHandle LogScoring $ plainShow s
    let m' = Model { _modelItems = map VarDeclI (topLevelVarDecls m)
                                   ++ functionItems m
                                   ++ annotationItems m
                                   ++ solutionToAssignments (s)
                                   ++ [ i | i@(ConstraintI _) <- m ^. modelItems ]
                                   ++ maybe [] ((:[]) . ConstraintI) maybeContext
                   }

    SimpleLog.log logHandle LogScoring $ "model to be tested for satisfiability:"
    SimpleLog.log logHandle LogScoring $ plainShow m'

    res <-
      timeAction "solveReplacement/checkSols/modelIsSatisfiable" $
      liftIO $ try (return $! modelIsSatisfiable env m')
    statisticsEvaluation
    SimpleLog.log logHandle LogScoring $ "satisfies: " ++ (show res)
    let succ' = case res of
                  Left (AssertFailed _) -> False
                  Left (EvalNotPar _) -> False
                  Left (EvalUndefined _) -> False
                  Left (UnfixedVariable _) -> False
                  Left (IndexOutOfRange _) -> False
                  Right b -> b
    return succ'
  let goods = length (filter id results)
  statisticsEvaluationAdd (length csols)
  void $ liftIO $ evaluate goods
  SimpleLog.log logHandle LogScoring $ "goods: " ++ prettyPrintify c ++ ": " ++ show goods
  return $! (c, fromIntegral goods / fromIntegral nSampleSols :: Double)

getGoodConstraints :: String -> Bindings -> Maybe (Expression) -> Model -> [Replacement] -> GOpts.GlobalizerOptions -> SimpleLog.Handle -> StatisticsIO (Maybe [(Replacement,Double)])
getGoodConstraints dataFilePath env maybeContext m inter opts logHandle = do
  SimpleLog.log logHandle LogDebug "get good constraints"
  zincCalls <- liftIO $ newTMVarIO (0::Int)
  evalCalls <- liftIO $ newTMVarIO (0::Int)
  when (length inter > 0) $ do
    SimpleLog.logN logHandle LogHigh $ "(" ++ show (length inter) ++ ") "
    forM_ inter $ \c -> do
      SimpleLog.log logHandle LogHigh $ prettyPrintify c

  let testConstraint c = do
        let outVars = getVariablesInConstraints m
        scoresAndContexts <- do
          result <- timeScoreReplacement dataFilePath env maybeContext m outVars c opts logHandle
          SimpleLog.logN logHandle LogHigh $ "."
          return result
        return $! scoresAndContexts

  let isTrue (c,_) = name c == "true"
  exitEarly <- (const False) <$>
    case find isTrue inter of
      Nothing -> return False
      Just c -> do
        -- Let's just check true first, because if it succeeds we
        -- don't need to check any others -- they'll all succeed.
        (_, trueScore) <- testConstraint c
        return $ trueScore >= 1.0

  if exitEarly
    then return Nothing
    else do
      scores <- forM inter $ \c -> statisticsTime (T.pack (show c)) $ do
        testConstraint c

      when (length inter > 0) $
        SimpleLog.log logHandle LogHigh $ ""

      liftIO (atomically (takeTMVar zincCalls)) >>= statisticsFlatZincCallAdd
      liftIO (atomically (takeTMVar evalCalls)) >>= statisticsEvaluationAdd

      let goodnessThreshold = 1.0
          isGood (_c,s) = s >= goodnessThreshold
      let rockSolidConstraints = filter isGood $ scores

      void $ liftIO $ evaluate (length rockSolidConstraints)
      return . Just $! rockSolidConstraints

annotationItems :: Model -> [Item]
annotationItems m = [ i | i@(AnnotationI {}) <- m ^. modelItems ]

functionItems :: Model -> [Item]
functionItems m = [ i | i@(FunctionI {}) <- m ^. modelItems ]

compareReplacementAndContext :: (Replacement,Double)
                             -> (Replacement,Double)
                             -> Ordering
compareReplacementAndContext (_r1,score1) (_r2,score2) =
    case compare score1 score2 of
      LT -> LT
      GT -> GT
      EQ -> EQ

prettyPrintify :: Replacement -> String
prettyPrintify (Constraint { name=n }, args) =
  constraintName n ++ "(" ++ intercalate "," (map f args) ++ ")"
  where f (OrdinaryParameter (Ident i)) = i
        f (OrdinaryParameter (IntLit x)) = show x
        f (OrdinaryParameter (ArrayLit es [_])) = "[" ++ intercalate ", " (map (f . OrdinaryParameter . view expRawExpression) es) ++ "]"
        f (OrdinaryParameter (ArrayAccess (expRaw -> Ident a) idx)) =
            a ++ "[" ++ intercalate "," (map (f . OrdinaryParameter . view expRawExpression) idx) ++ "]"
        f (ErstwhileVariable i) = i
        f (ArgumentArrayAccess a idx) = f a ++ "[" ++ intercalate ", " (map (f . OrdinaryParameter) idx) ++ "]"
        f x = show x


tightest :: Bindings -> [(Replacement,Double)] -> [(Replacement,Double)]
tightest env cs = 
  filter (\(c,_s) -> not (any (\c2 -> tighter env c2 c) (map (view _1) cs))) cs

tighter :: Bindings -> Replacement -> Replacement -> Bool
tighter env c1 c2 = tighter1 env c1 c2 || tighter2 env c1 c2

tighter1 :: Bindings -> Replacement -> Replacement -> Bool
tighter1 _env (c1,args1) (c2,args2) =
    name c1 == name c2
    && f args1 args2
  where f [] [] = False
        f (a1:as1) (a2:as2) | a1 == a2 = f as1 as2
                            | otherwise = False
        f _ _ = error "tighter1"

tighter2 :: Bindings -> Replacement -> Replacement -> Bool
tighter2 _env (Constraint {name="count"},[x1,v1,c1]) (Constraint {name="exactly"},[c2,x2,v2]) =
    x1 == x2 && v1 == v2 && c1 == c2
tighter2 _ _ _ = False


strongestReplacements :: Bindings -> [Replacement] -> [Replacement]
strongestReplacements env rs = filter (\r -> not (any (\r2 -> stronger env r2 r) rs)) rs

stronger :: Bindings -> Replacement -> Replacement -> Bool
stronger env (Constraint {name="atleast"},[OrdinaryParameter n,x,v]) (Constraint {name="atleast"},[OrdinaryParameter n',x',v']) =
 grInt env n n' && x == x' && v == v'
stronger env (Constraint {name="atmost"},[OrdinaryParameter n,x,v]) (Constraint {name="atmost"},[OrdinaryParameter n',x',v']) =
 leInt env n n' && x == x' && v == v'
stronger env (Constraint {name="sliding_sum"},[OrdinaryParameter l,OrdinaryParameter u,OrdinaryParameter n,x]) (Constraint {name="sliding_sum"},[OrdinaryParameter l',OrdinaryParameter u',OrdinaryParameter n',x']) =
  eqInt env n n' && x == x'
  && ( ( eqInt env l l' && leInt env u u' ) || ( eqInt env u u' && grInt env l l' ) )
stronger _ _ _ = False

opInt :: Bindings
      -> (Expression' -> Expression' -> t)
      -> Expression'
      -> Expression'
      -> t
opInt env bop x y = case (expressionToValue env x, expressionToValue env y) of
                     (IntLit _, IntLit _) -> bop x y
                     _ -> error "opInt"
eqInt,leInt,grInt :: Bindings -> Expression' -> Expression' -> Bool
eqInt env = opInt env (==)
leInt env = opInt env (<)
grInt env = opInt env (>)

instantiate :: Replacement -> [Item]
instantiate (c,[n,x]) | name c == "sum_constraint" =
  [ ConstraintI (makeExp $ BinOp (instantiate' n) BinOpEq (makeExp $ Call "sum" [instantiate' x])) ]
instantiate (c,[s,t,x]) | name c == "value_precede_checking" =
  [ ConstraintI (makeExp $ Call "value_precede_int" (map instantiate' [s,t,x])) ]
instantiate (c,[x,y]) | name c == "lex_less_int_checking" =
  [ ConstraintI (makeExp $ Call "lex_less_int" (map instantiate' [x,y])) ]
instantiate (c,[x,y]) | name c == "lex_lesseq_int_checking" =
  [ ConstraintI (makeExp $ Call "lex_lesseq_int" (map instantiate' [x,y])) ]
instantiate (c,[x]) | name c == "lex2_checking" =
  [ ConstraintI (makeExp $ Call "lex2" (map instantiate' [x])) ]
instantiate (c,a) = [ ConstraintI (makeExp $ Call (constraintName (name c)) (map instantiate' a)) ]
  
instantiate' :: Argument -> Expression
instantiate' (OrdinaryParameter e) = makeExp e
instantiate' (ErstwhileVariable vid) = makeExp $ Ident vid
instantiate' (ArgumentArrayAccess a idx) = makeExp $ ArrayAccess (instantiate' a) (map makeExp idx)
instantiate' Blank = error "instantiate'"
        

constraintName :: String -> String
constraintName "sum_constraint" = "sum"
constraintName "maximum_int_checking" = "maximum"
constraintName "minimum_int_checking" = "minimum"
constraintName "subcircuit_checking" = "subcircuit"
constraintName "circuit_checking" = "circuit"
constraintName "sort_checking" = "sort"
constraintName x = x


core :: Constraint -> [Expression] -> [Item]
core c args = [ ConstraintI (makeExp $ Call (name c) args) ]

extra :: String -> [Expression] -> [Item]
extra "alldifferent" [x] = notSingleton x
extra "alldifferent_except_0" [x] =
  extra "alldifferent" [x]
  ++ [ ConstraintI (makeExp $ Call "member" [x,makeExp $ IntLit 0]) ]
  ++ [ ConstraintI (makeExp $ Call "atmost" [makeExp $ BinOp (makeExp $ Call "length" [x]) BinOpMinus (makeExp $ IntLit 2),x,makeExp $ IntLit 0]) ]
extra "all_equal_int" [x] = notSingleton x
extra "atleast" [n,x,_v] =
    [ ConstraintI (makeExp $ BinOp n BinOpLe (makeExp $ Call "length" [x])) ]
    ++ [ ConstraintI (makeExp $ BinOp n BinOpGr (makeExp $ IntLit 0)) ]
extra "atmost" [n,xs,_v] =
    [ ConstraintI (makeExp $ BinOp n BinOpLe (makeExp $ Call "length" [xs])) ]
    ++ notSingleton xs
    ++ [ ConstraintI (makeExp $ BinOp n BinOpGr (makeExp $ IntLit 0)) ]
extra "bin_packing" [_c,x,_w] =
    [ ConstraintI (makeExp $ BinOp (makeExp $ IntLit 20) BinOpGq
                             (makeExp $ BinOp (makeExp $ Call "ub_array" [x]) BinOpMinus
                                              (makeExp $ Call "lb_array" [x]))) ]
extra "channelACB" [x,b] =
    [ ConstraintI (makeExp $ BinOp (makeExp $ Call "index_set_1of2" [x]) BinOpEq (makeExp $ Call "index_set_1of3" [b]))
    , ConstraintI (makeExp $ BinOp (makeExp $ Call "index_set_2of2" [x]) BinOpEq (makeExp $ Call "index_set_3of3" [b]))
    ]
extra "count" [_xs,_v,c] =
    [ ConstraintI (makeExp $ BinOp c BinOpGr (makeExp $ IntLit 0)) ]
extra "count_geq" [_xs,_v,_c] = [ ]
extra "cumulative_assert" [s,d,r,b] =
    notSingleton s ++ notSingleton d ++ notSingleton r
    ++ [ ConstraintI (makeExp $ BinOp b BinOpGr (makeExp $ IntLit 0) ) ]
extra "decreasing" [x] = [ ConstraintI (makeExp $ BinOp (makeExp $ Call "length" [x]) BinOpGr (makeExp $ IntLit 2)) ]
extra "exactly" [n,_xs,_v] =
    [ ConstraintI (makeExp $ BinOp n BinOpGr (makeExp $ IntLit 0)) ]
extra "gcc" [xs, vs] =
    notSingleton xs ++ notSingleton vs
extra "increasing" [x] = [ ConstraintI (makeExp $ BinOp (makeExp $ Call "length" [x]) BinOpGr (makeExp $ IntLit 2)) ]
extra "lex_less_int_checking" [xs,ys] = 
    [ ConstraintI (makeExp $ BinOp (makeExp $ Call "length" [xs]) BinOpEq (makeExp $ Call "length" [ys])) ]
    ++ notSingleton xs
extra "lex_lesseq_int_checking" [xs,ys] = 
    [ ConstraintI (makeExp $ BinOp (makeExp $ Call "length" [xs]) BinOpEq (makeExp $ Call "length" [ys])) ]
    ++ notSingleton xs
extra "lineareq" [xs,ys,_n] = 
    [ ConstraintI (makeExp $ BinOp (makeExp $ Call "length" [xs]) BinOpEq (makeExp $ Call "length" [ys])) ]
extra "nvalue" [n,xs] = 
    [ ConstraintI (makeExp $ BinOp n BinOpLe (makeExp $ Call "length" [xs])) ]
extra "sliding_sum" [l,u,n,xs] =
    [ ConstraintI (makeExp $ BinOp l BinOpLe u)
    , ConstraintI (makeExp $ BinOp n BinOpLe (makeExp $ Call "length" [xs]))
    , ConstraintI (makeExp $ BinOp n BinOpGr (makeExp $ IntLit 1))
    -- The below constraint is too strict
    -- , ConstraintI (makeExp $ BinOp (makeExp $ BinOp u BinOpLe (makeExp $ BinOp n BinOpMult (makeExp $ Call "ub_array" [xs])))
    --                      BinOpOr
    --                      (makeExp $ BinOp l BinOpGr (makeExp $ BinOp n BinOpMult (makeExp $ Call "lb_array" [xs]))))
    ]
extra "value_precede_checking" [s,t,x] =
    [ ConstraintI (makeExp $ Call "member" [x,s])
    , ConstraintI (makeExp $ Call "member" [x,t]) ]
extra _cname _args = []

notSingleton :: Expression -> [Item]
notSingleton x = [ ConstraintI (makeExp $ BinOp (makeExp $ Call "length" [x]) BinOpGr (makeExp $ IntLit 1)) ]

check :: Bindings -> Int -> String -> [Argument] -> Bool
check _ _nsols "alldifferent" [x] = isVariable x -- && is1DArray bs x
check _ _nsols "alldifferent_except_0" [x] = isVariable x -- && is1DArray bs x
check _ _nsols "all_equal_int" [x] = isVariable x -- && is1DArray bs x
check _ _ "atleast" [_,x,_] = {-# SCC "insideCheck" #-} isVariable x -- && is1DArray bs x
--                                                  && isInt bs n && isInt bs v
--                                                  && appearsIn bs nsols v x
check bs _ "atmost" [n,x,_] = isVariable x -- && is1DArray bs x
                                                 && isInt bs n -- && isInt bs v
--                                                 && appearsIn bs nsols v x
check _ _nsols "bin_packing" [c,bin,w] = isNotVariable c -- && isInt bs c
                                                  && isVariable bin -- && is1DArray bs bin
                                                  && isNotVariable w -- && is1DArray bs w
check _ _nsols "bin_packing_capa" [c,bin,w] = isNotVariable c -- && is1DArray bs c
                                                       && isVariable bin -- && is1DArray bs bin
                                                       && isNotVariable w -- && is1DArray bs w
check _ _nsols "bin_packing_load" [l,bin,w] = isVariable l -- && is1DArray bs l
                                                       && isVariable bin -- && is1DArray bs bin
                                                       && isNotVariable w -- && is1DArray bs w
check _ _nsols "bin_packing_load_ub" [l,bin,w] = isVariable l -- && is1DArray bs l
                                                         && isVariable bin -- && is1DArray bs bin
                                                         && isNotVariable w -- && is1DArray bs w
check _ _nsols "binaries_represent_int" [b] = isVariable b
check _ _nsols "binaries_represent_int_3A" [b] = isVariable b
check _ _nsols "binaries_represent_int_3B" [b] = isVariable b
check _ _nsols "binaries_represent_int_3C" [b] = isVariable b
check _ _nsols "channel" [x,a] =
--    is1DArray bs a && isInt bs x
    (isVariable a || isVariable x)
check _ _nsols "channelACB" [x,a] =
--    is1DArray bs a && isInt bs x
    (isVariable a || isVariable x)
check _ _nsols "circuit_checking" [x] = isVariable x -- && is1DArray bs x
check _ _nsols "count" [x,_,_] = isVariable x -- && is1DArray bs x
                                          -- && isInt bs y
                                          -- && isInt bs c
check _ _nsols "count_geq" [x,_,_] =
  -- FORBIDDEN
  False && isVariable x
check _ _nsols "cumulative" [s,d,r,_] =
    (isVariable s || isVariable d || isVariable r) -- Implement isPositive
    && isPositive d && isPositive r
    && s /= d && d /= r && s /= r
check bs nsols "cumulative_assert" args = check bs nsols "cumulative" args
check _ _nsols "decreasing" [x] = isVariable x -- && is1DArray bs x
check _ _nsols "diffn" [x,y,dx,dy] =
    -- is1DArray bs x
    -- && is1DArray bs y
    -- && is1DArray bs dx
    -- && is1DArray bs dy
      ((isVariable x && isVariable y) || (isVariable dx && isVariable dy))
    && x /= dx && x /= dy && y /= dx && y /= dy
    && x /= y
check _ _nsols "distribute" [_,_,_] = True 
                                       -- && is1DArray bs c
                                       --         && is1DArray bs v
                                       --         && is1DArray bs b
check _ _nsols "element" [_,_,_] =
  -- FORBIDDEN
  False && True -- && isInt bs i && isInt bs v
                                         --    && is1DArray bs a
check _ _nsols "exactly" [n,x,v] = isNotVariable n -- && isInt bs n
                                            && isVariable x -- && is1DArray bs x
                                            && isNotVariable v -- && isInt bs v
check bs _nsols "gcc" [x,_] = is1DArray bs x -- && is1DArray bs cs
check _ _nsols "global_cardinality" [_,cv,_] = -- is1DArray bs x
                                                         isNotVariable cv -- && is1DArray bs cv
                                                         -- && is1DArray bs cs
check _ _nsols "increasing" [x] = isVariable x -- && is1DArray bs x
check _ _nsols "inverse" [_,_] = True -- && is1DArray bs x && is1DArray bs y
check _ _nsols "lex_less_int_checking" [_,_] = True -- && is1DArray bs x && is1DArray bs y
check _ _nsols "lex_lesseq_int_checking" [x,y] = x /= y -- && is1DArray bs x && is1DArray bs y
check _ _nsols "lex2" [x] = isVariable x -- && is2DArray bs x
check bs nsols "lex2_checking" [x] = check bs nsols "lex2" [x]
check _ _nsols "lineareq" [_,_,_] = True -- && is1DArray bs x && is1DArray bs y
                                          --    && isInt bs n
check _ _nsols "member" [_,_] =
  -- FORBIDDEN
  False && True -- && is1DArray bs xs && isInt bs x
check _ _nsols "maximum_int_checking" [_,_] = True -- && is1DArray bs xs && isInt bs x
check _ _nsols "minimum_int_checking" [_,_] = True -- && is1DArray bs xs && isInt bs x
check _ _nsols "nvalue" [_,_] = True -- && is1DArray bs x && isInt bs n
check _ _nsols "sliding_sum" [l,u,s,vs] = isNotVariable l -- && isInt bs l
                                                   && isNotVariable u -- && isInt bs u
                                                   && isNotVariable s -- && isInt bs s
                                                   && (l /= u) && (l /= s) && (u /= s)
                                                   && isVariable vs -- && is1DArray bs vs
check _ _nsols "sort_checking" [x,y] = -- is1DArray bs x && is1DArray bs y
                                                 (isVariable x || isVariable y)
check bs nsols "strict_lex2" [x] = check bs nsols "lex2" [x]
check bs nsols "strict_lex2_checking" [x] = check bs nsols "strict_lex2" [x]
check _ _nsols "subcircuit_checking" [x] = isVariable x -- && is1DArray bs x
--check bs nsols (Constraint "sum_constraint" 2) [n,x] | trace (show (n,x)) False = undefined
check _ _nsols "sum_constraint" [_,x] = isVariable x -- && is1DArray bs x
                                      -- && isInt bs n
check _ _nsols "unary" [s,d] =
    -- is1DArray bs s
    -- && is1DArray bs d
     (isVariable s || isVariable d)
check _ _ "value_precede" [s,t,x] = isNotVariable s -- && isInt bs s
                                                  && isNotVariable t -- && isInt bs t
                                                  && isVariable x -- && is1DArray bs x
--                                                  && (appearsIn bs nsols s x && appearsIn bs nsols t  x)
                                                  && s /= t
check bs nsols "value_precede_checking" args = check bs nsols "value_precede" args
check _bs _nsols _cname _args = True

isVariable :: Argument -> Bool
isVariable (ErstwhileVariable {}) = True
isVariable (ArgumentArrayAccess a _) = isVariable a
isVariable _ = False
isNotVariable :: Argument -> Bool
isNotVariable = not . isVariable

isPositive :: Argument -> Bool
isPositive a = case instantiate' a ^. expRawExpression of
                 IntLit x -> x > 0
                 _ -> False

-- Make sure at least one argument is a variable.
argCheck :: [Argument] -> Bool
argCheck args = any isVariable args

noBlanks :: [Argument] -> Bool
noBlanks = not . any blank
  where blank Blank = True
        blank _     = False

evalModelIdentifiers :: Model -> Model
evalModelIdentifiers m =
  snd $ runResolve $ do
    me <- resolveModel m
    (vdidmap, _bmap) <- get
    let f :: Expression -> Expression
        f e@(expRaw -> Bound _ (VarDeclId vdid)) =
          let Just vd = IM.lookup vdid (idMap vdidmap)
          in case vd ^. varDeclExpression of
               Nothing -> e
               Just rhs -> f (e & expRawExpression .~ (expRaw rhs))
        f e = e
    let me2 = transformOnOf template template f me
    return me2


getVariablesInConstraints :: Model -> [Expression']
getVariablesInConstraints m' =
  let m = evalModelIdentifiers . evalModelArraySlices $ m'
      vars = concatMap variablesDefinedByItem (m ^. modelItems)
  in nub vars

variablesDefinedByItem :: Item -> [Expression']
variablesDefinedByItem (VarDeclI vd) = do
  let ti = vd ^. varDeclTypeInst
      ident = vd ^. varDeclIdent
  guard (tiInst ti == Var)
  return $ Ident ident
variablesDefinedByItem _ = []

chooseIndex :: TypeInst -> [Expression]
chooseIndex ti =
  case tiDomain ti of
    Just e -> case e ^. expRawExpression of
                SetLit (SLVIntSetVal [(l,u)]) -> map (makeExp . IntLit) [l..u]
                BinOp le BinOpDotdot ue ->
                    let IntLit l = le ^. expRawExpression
                        IntLit u = ue ^. expRawExpression
                    in map (makeExp . IntLit) [l..u]
                x -> error $ "chooseIndex: " ++ show x
    Nothing -> undefined

-- This is a specialised version of "sequence", but has better memory
-- behaviour.  (For more information see
-- http://www.cmears.id.au/articles/sequence-space-leak.html)
cartesianProduct1 :: [[a]] -> [[a]]
cartesianProduct1 = cartesianProduct' [] . reverse
  where cartesianProduct' :: [a] -> [[a]] -> [[a]]
        cartesianProduct' acc [] = [acc]
        cartesianProduct' acc (xs:xss) =
            concatMap (\x -> cartesianProduct' (x:acc) xss) xs

-- Does the model have exactly two constraints, and does one
-- constraint seem to imply the other?
impliesCheck :: String -> Bindings -> Model -> GOpts.GlobalizerOptions -> SimpleLog.Handle -> StatisticsIO Bool
impliesCheck dataFilePath env m opts logHandle = do
    SimpleLog.log logHandle LogSolving "Starting implies check"
    result <-
      case filter isConstraintI (m ^. modelItems) of
        [c1,c2] -> do let m' = m & modelItems %~ filter (not . isConstraintI)
                      i1 <- impliesCheck2 dataFilePath env m' c1 c2 opts logHandle
                      if i1 then return True
                        else impliesCheck2 dataFilePath env m' c2 c1 opts logHandle
        _ -> return False
    SimpleLog.log logHandle LogSolving $ "Implies check returns: " ++ show result
    return result

-- See if C1 implies C2 in the context of model M (which has no other
-- constraints).
impliesCheck2 :: String -> Bindings -> Model -> Item -> Item -> GOpts.GlobalizerOptions -> SimpleLog.Handle -> StatisticsIO Bool
impliesCheck2 dataFilePath env m c1 c2 opts logHandle = do
    let nRandomSols = GOpts.nRandomSolutions opts
    -- Find some solutions to M+C1.
    SimpleLog.log logHandle LogDebug "impliesCheck2"
    (_, sols1) <-
      timeAction "impliesCheck2/solve" $
      solve dataFilePath True (m & modelItems %~ (c1:)) opts logHandle
    if length sols1 < nRandomSols
      then do
        -- Didn't get enough solutions to determine implication.
        -- liftIO $ hPutStrLn stderr "implies check failed due to termination"
        return False
      else do
        SimpleLog.logPrint logHandle LogDebug sols1
        let f [] = return True
            f (s:ss) = do
              -- Construct M+C2+(a solution to C1)
              let m' = m & modelItems %~ (++ ([c2] ++ solutionToAssignments s))
              -- See if M+C2+(solution to C1) is satisfiable.
              SimpleLog.log logHandle LogDebug $ "checking for satisfiability:"
              SimpleLog.log logHandle LogDebug $ plainShow m'
              res <- liftIO $ try (return $! modelIsSatisfiable env m')
              statisticsEvaluation
              let succ' = case res of
                                Left (AssertFailed _) -> False
                                Left (EvalNotPar _) -> False
                                Left (EvalUndefined _) -> False
                                Left (UnfixedVariable _) -> False
                                Left (IndexOutOfRange _) -> False
--                                    error $ "uh oh: " ++ show (IndexOutOfRange msg)
                                Right b -> b

              if succ' then f ss else return False
        f (sols1)

solutionToAssignments :: Model -> [Item]
solutionToAssignments = _modelItems

fromRight :: (Show e) => Either e a -> a
fromRight = either (error . show) id

parseItem :: String -> Item
parseItem = either (error . show) id . parseString item

freeIdentifiersInConstraints :: Model -> [VarId]
freeIdentifiersInConstraints m =
    concatMap unboundIdentifiersAfterResolution (modelConstraints m)

modelConstraints :: Model -> [Expression]
modelConstraints (Model items) = [ c | ConstraintI c <- items ]

-- Delete a VarDecl's initialisation expression
uninitialise :: VarDecl -> VarDecl
uninitialise = varDeclExpression .~ Nothing

data FailedProcessReason =
    NoSolutions
  | ImpliesCheck
  | TooBig
  deriving (Show, Typeable)
instance Exception FailedProcessReason
failedProcessReason :: Prism' SomeException FailedProcessReason
failedProcessReason = prism' SomeException fromException

processModelWrapper :: String
                    -> Bindings
                    -> Maybe (Expression)
                    -> Model
                    -> Maybe [(Replacement,Double)]
                    -> GOpts.GlobalizerOptions
                    -> Maybe String
                    -> ChannelMap
                    -> SimpleLog.Handle
                    -> StatisticsIO [(Replacement,Double)]
processModelWrapper dataFilePath env maybeContext m maybeReps opts consFilter channelMap logHandle = do
  let innerHandler action = 
          catching failedProcessReason action $ \_ -> do
            return []
      outerHandler :: StatisticsIO [(Replacement,Double)] -> StatisticsIO [(Replacement,Double)]
      outerHandler action = catch action $ \err -> do
                              _ <- liftIO $ hPutStrLn stderr $ "processModelWrapper caught: " ++ show (err::SomeException)
                                             ++ "\nin this model:\n" ++ plainShow m
                              return []
  outerHandler $ innerHandler $ processModel dataFilePath env maybeContext m maybeReps opts consFilter channelMap logHandle

-- Prepare a model to have its solutions sampled.
prepareModel :: String -> Bindings -> Model -> GOpts.GlobalizerOptions -> SimpleLog.Handle -> StatisticsIO (Model)
prepareModel dataFilePath env m opts logHandle = do
  let doImpliesChk = GOpts.doImpliesCheck opts
  -- Alter the search annotation and output item.
  let modelToSolve = replaceOutput . (replaceSearch opts) $ m 

  -- If the model has only two constraints and one implies the other, give up.
  statisticsTime "implies check" $ do
    implies <-
        if doImpliesChk
        then impliesCheck dataFilePath env modelToSolve opts logHandle
        else return False
    when implies $ do
      SimpleLog.log logHandle LogSolving $ "implies check succeeded on this model: " ++ plainShow modelToSolve
      throwingM failedProcessReason ImpliesCheck
  return modelToSolve

splitFilter :: String -> [String]
splitFilter consFilter = map (\t -> T.unpack t) (T.splitOn (T.pack ",") (T.pack consFilter))

anyInfix :: String -> String -> Bool
anyInfix consFilter call_id = any (\s -> (s `isInfixOf` call_id)) (splitFilter consFilter)

processModel :: String
             -> Bindings
             -> Maybe (Expression)
             -> Model
             -> Maybe [(Replacement,Double)]
             -> GOpts.GlobalizerOptions
             -> Maybe String
             -> ChannelMap
             -> SimpleLog.Handle
             -> StatisticsIO [(Replacement,Double)]
processModel dataFilePath env maybeContext m' maybeReps opts consFilter channelMap logHandle = do
  let nRandomSols = GOpts.nRandomSolutions opts
  let filterArgs = GOpts.filterArguments opts
  let maxArgs = GOpts.maxArguments opts
  -- Add the context to the model.
  let m = Language.MiniZinc.evalModelArraySlices (m' & modelItems %~ (++ (maybe [] ((:[]) . ConstraintI) maybeContext)))

  SimpleLog.log logHandle LogHigh $ "about to solve model"

  modelToSolve <- prepareModel dataFilePath env m opts logHandle

  (result, solutions) <- statisticsTime "solving model" $ do
    SimpleLog.log logHandle LogSolving $ "solving model..."
    SimpleLog.log logHandle LogSolving $ plainShow modelToSolve
    -- Find some random solutions.
    (result, solutions') <-
      timeAction "processModel/solve" $
      solve dataFilePath True modelToSolve opts logHandle
    SimpleLog.log logHandle LogSolving "done solving model, counting solutions..."
    void $ liftIO $ evaluate $ length solutions'
    SimpleLog.log logHandle LogSolving $ "there are " ++ show (length solutions') ++ " solutions (" ++ show result ++ ")"
    return (result, solutions')
  let solutionAssignments = map solutionToAssignments solutions
  forM_ solutions $ \sol -> do
    SimpleLog.log logHandle LogSolving $ "solution: " ++ plainShow sol

  when (result == SolveIncomplete && (nRandomSols == 0 || length solutions < nRandomSols)) $ do
    SimpleLog.log logHandle LogSolving $ "not enough solutions to this model"
    SimpleLog.log logHandle LogSolving $ "(found only " ++ show (length solutions) ++ ")"
    throwingM failedProcessReason NoSolutions

  when (result == SolveComplete && length solutions == 0) $ do
    SimpleLog.log logHandle LogSolving $ "model proved unsatisfiable"
    throwingM failedProcessReason NoSolutions

  SimpleLog.log logHandle LogHigh $ "model solved"

  -- Now we construct the "template" model for the constraint checking
  -- part.  This model has all the declarations of the original model,
  -- plus declarations for the variables that are actually "used" in
  -- the output.
  -- 
  -- Example.  If we have an output statement like:
  --
  --   output [ x[1], x[2], x[3] ]
  --
  -- then "x" is an output variable identifier.  We will have the
  -- original variable declaration for x, and a new declaration
  -- "x_used" for the three variables in the output:
  --
  --   array [1..n] of int : x;
  --   array [1..3] of int : x_used = [x[1],x[2],x[3]];

  let -- List of all original declarations.
      allVarDecls = topLevelVarDecls m

  -- To construct the template, we convert all variables into
  -- parameters.  At this point they do not have any value -- these
  -- will be inserted later.
  let constraintTemplateModel0 =
          Model { _modelItems = map (VarDeclI . makePar) allVarDecls }

  SimpleLog.log logHandle LogConstraints $ "constraintTemplateModel0:"
  SimpleLog.log logHandle LogConstraints $ plainShow constraintTemplateModel0

  -- We also need to include all the function items from the original
  -- model.  Without them, we might not be able to evaluate the model
  -- to test for satisfiability.
  let constraintTemplateModel =
        constraintTemplateModel0 & modelItems <>~ filter isFunctionI (m ^. modelItems)

  let firstSolution = headNote "no solutions to submodel?" solutions
  let identifiersInSolution = --map firstWord (lines (firstSolution))
        map (\(AssignI x _) -> x) (_modelItems firstSolution)

  -- At this point, constraintTemplateModel' no longer has the
  -- original declarations for variables --- they have been replaced
  -- by the per-solution versions.

  -- Now we add a default solve item and include the globals.
  let constraintTemplateModel'' =
        constraintTemplateModel & modelItems <>~
                                    [SolveI mempty Nothing SolveSatisfy] ++
                                    [IncludeI "globals.mzn" Nothing, IncludeI "gecode.mzn" Nothing]
  let templateEnv = topLevelBindings constraintTemplateModel''

  -- The constraint expressions, including the context (if any).
  let constraintExpressions = [ e | ConstraintI e <- m ^. modelItems ] ++ maybe [] (:[]) maybeContext

  -- Glue all the constraint expressions into one big fat nonsense
  -- expression.  Traversing this will give us all the expressions
  -- appearing in any constraint.
  let allConstraintsExpression = makeExp $ Call "?" constraintExpressions

  SimpleLog.log logHandle LogConstraints $ "constraintExpressions:"
  forM_ constraintExpressions $ \e ->
    (SimpleLog.log logHandle LogConstraints $ showExp2 e)
  SimpleLog.log logHandle LogConstraints $ "allConstraintsExpression:"
  SimpleLog.log logHandle LogConstraints $ (showExp2 allConstraintsExpression)

  -- Get all the "par" variable declarations.  These are used to
  -- compute possible arguments for global constraints.
  let parVarDecls = filter isPar $ allVarDecls

  SimpleLog.log logHandle LogArgs $ "about to compute potential arguments..."
  potentialArguments <- computePotentialArguments templateEnv identifiersInSolution
                                                  parVarDecls allConstraintsExpression
                                                  filterArgs  channelMap logHandle

  let baseEnv = topLevelBindings m

  let typeInstToArgType ti =
          case ti of
               TypeInst { tiRanges = OrdinaryRanges rs, tiBase = BTInt, tiSet = Plain } -> ArgType ArgInt (genericLength rs)
               TypeInst { tiRanges = OrdinaryRanges rs, tiBase = BTInt, tiSet = Set } -> ArgType ArgSetInt (genericLength rs)
               TypeInst { tiRanges = OrdinaryRanges rs, tiBase = BTUnknown,
                          tiDomain = Just (expRaw -> Ident i) } ->
                            fromMaybe (error ("couldn't look up \"" ++ i ++ "\"")) $ do
                              return $ ArgType ArgInt (genericLength rs)
               ti' -> error $ "getArgTypeIdent: unknown typeinst: " ++ show ti'

  let getArgTypeIdent i =
          let ti = fromMaybe (error ("getArgTypeIdent: " ++ i)) $ lookupTypeInst i baseEnv
          in typeInstToArgType ti

  let isIntLit (expRaw -> IntLit _) = True
      isIntLit _ = False
      getArgType (OrdinaryParameter (IntLit _)) = Just $ ArgType ArgInt 0
      getArgType (OrdinaryParameter (Ident i)) = Just $ getArgTypeIdent i
      getArgType (OrdinaryParameter (ArrayLit (e:_) [_])) | isIntLit e = Just $ ArgType ArgInt 1
      getArgType (ErstwhileVariable i) = Just $ getArgTypeIdent i
      getArgType (ArgumentArrayAccess a e's) = do
          let properIndices = filter (/= (Ident "_")) e's
              nProperIndices = genericLength properIndices
              (ArgType bt dims) = fromMaybe (error "getArgType") $ getArgType a
          guard (dims >= nProperIndices)
          guard $ all (\e' -> fromMaybe (error "getArgType") (getArgType (OrdinaryParameter e')) == ArgType ArgInt 0) properIndices
          return $ ArgType bt (dims - nProperIndices)
      getArgType x = error $ "getArgType: " ++ show x

  let typecheck c args =
          let expectedArgTypes = argtypes c
              argType = mapM getArgType args
          in maybe False (==expectedArgTypes) argType

  SimpleLog.log logHandle LogDebug "parVarDecls"
  mapM_ (SimpleLog.logPrint logHandle LogDebug) parVarDecls
  SimpleLog.log logHandle LogArgs "model:"
  SimpleLog.log logHandle LogArgs (plainShow m)
  SimpleLog.log logHandle LogArgs "context:"
  SimpleLog.log logHandle LogArgs (maybe "<no context>" showExp maybeContext)
  SimpleLog.log logHandle LogArgs "potentialArguments"
  let showArg Blank = "<blank>"
      showArg a = show (getArgType a) ++ ": " ++ show a
  mapM_ (SimpleLog.log logHandle LogArgs . showArg) potentialArguments
  SimpleLog.log logHandle LogArgs "(end of potential args)"

  let argumentsByType = M.fromListWith (++) [ (t,[a]) | a <- potentialArguments, let mt = getArgType a, isJust mt, let Just t = mt ]

  let acceptableConstraint (Constraint name' _) =
          and [ name' /= "atmost"
              , name' /= "atleast"
              , case consFilter of
                  Nothing -> True
                  Just s -> anyInfix s name' ]

  let constraintsToConsider = filter acceptableConstraint potentialConstraints

  SimpleLog.log logHandle LogHigh $ "considering " ++ show (length constraintsToConsider) ++ " constraints"

  replacements <- forM constraintsToConsider $ \c -> statisticsTime (T.pack (show c)) $ do
   flip (catching failedProcessReason) (\fpr -> do case fpr of
                                                     TooBig -> -- liftIO (hPutStrLn stderr ("(too big - " ++ show (name c) ++ ")")) >>
                                                                 return []
                                                     otherReason -> throwingM failedProcessReason otherReason) $ do
    SimpleLog.log logHandle LogConstraints $ "considering constraint " ++ name c
    let chooseArgument t = Blank : M.findWithDefault [] t argumentsByType
    let cart = mapM chooseArgument (argtypes c)
        numArgLists = length cart
    SimpleLog.log logHandle LogArgs $ "for constraint " ++ (name c) ++ " there are " ++ show numArgLists ++ " argument lists"
    SimpleLog.log logHandle LogArgs $ "  " ++ take 100 (show cart)

    when (numArgLists > maxArgs) $ do
      throwingM failedProcessReason TooBig

    let rawargslist = cart
    evalCalls <- liftIO $ newTMVarIO (0::Int)
    replacements' <- statisticsTime "argumentlists" $ forM rawargslist $ \rawargs' -> statisticsTime (T.pack (show rawargs')) $ do

      let rawargs = rawargs'
      eitherArgs <- liftIO $ try $ return $! fillInBlanks c templateEnv solutionAssignments rawargs
      let args = case eitherArgs of
                   Left (IndexOutOfRange _) -> rawargs
                   Left _ -> rawargs
                   Right r -> r
      let typecheckResult = typecheck c args
      let checkResult = {-# SCC "insideCheckResult" #-} check templateEnv (length solutions) (name c) args
      recordLogKey "check result" (show (checkResult))
      SimpleLog.log logHandle LogChecking $ "PRELIMINARY CHECKING: " ++ show (c,args)
      let isInMaybeReps = maybe True (\reps -> (c,args) `elem` map (view _1) reps) maybeReps
      when (not isInMaybeReps) (SimpleLog.log logHandle LogChecking $ "Failure due to isInMaybeReps: " ++ show (c,args) ++ "\nlist: " ++ show maybeReps)
      let argcheckResult = argCheck args
      when (isInMaybeReps) $
        if not (noBlanks args)
        then SimpleLog.log logHandle LogChecking $ "no blanks: " ++ show False
        else if not checkResult
             then SimpleLog.log logHandle LogChecking $ "check result: " ++ show checkResult
             else if not argcheckResult
                  then SimpleLog.log logHandle LogChecking $ "argcheck result: " ++ show argcheckResult
                  else if not typecheckResult
                       then SimpleLog.log logHandle LogChecking $ "typecheck result: " ++ show typecheckResult
                       else return ()
      if isInMaybeReps &&
          noBlanks args && checkResult && (name c == "true" || argcheckResult) && typecheckResult
        then do
          let extraCon = concat [ (extra (name c)) args'
                                 | let args' = map argumentToExpression args ]
              coreCon = concat [ (core c) args'
                                 | let args' = map argumentToExpression args ]
          let items = extraCon ++ coreCon
          let modelToCheck = constraintTemplateModel'' & modelItems <>~ items
          recordLogKey "model to check" (plainShow modelToCheck)
          when (True || name c == "bin_packing_capa") $ SimpleLog.log logHandle LogChecking $ "CHECKING FOR: " ++ prettyPrintify (c,args) ++ " (there are " ++ show (length solutionAssignments) ++ " to check)"
          when (True || name c == "maximum_int_checking") $ SimpleLog.log logHandle LogChecking $ plainShow modelToCheck

          -- Try to find an assignment that makes the model unsatisfiable.
          findResult <- try $ findM (\assignments -> do
                                        statisticsEvaluation
                                        return $ not (modelIsSatisfiable' env (modelToCheck & modelItems <>~ assignments))) solutionAssignments

          liftIO $ atomically $ do x <- takeTMVar evalCalls
                                   putTMVar evalCalls (x+1)
          when (True || name c == "bin_packing_capa") $ SimpleLog.log logHandle LogChecking $
            case findResult of
              Left _ -> show findResult
              Right Nothing -> show findResult
              Right (Just items') -> "Right Just " ++ plainShow (Model items')
          let success2 = case findResult of
                           Left (AssertFailed _) -> False
                           Left (EvalNotPar _) -> False
                           Left (EvalUndefined _) -> False
                           Left (UnfixedVariable _) -> False
                           Left (IndexOutOfRange _) ->
                               if null [ () | OrdinaryParameter (ArrayAccess _ _) <- args ]
                                  && null [ () | ArgumentArrayAccess {} <- args ]
                               then False -- error $ "uh oh: " ++ show (IndexOutOfRange msg)
                               else False
--                           Right mb -> not (isJust mb)
                           -- We succeed if there is no such violating assignment.
                           Right mb -> isNothing mb
          when (True || name c == "bin_packing_capa") $ SimpleLog.log logHandle LogChecking $ show success2 ++ "\t      " ++ prettyPrintify (c,args)
          return $ if success2 then Just (c, args) else Nothing
        else return Nothing
    liftIO (atomically (takeTMVar evalCalls)) >>= statisticsEvaluationAdd
    return (catMaybes replacements')

  let result' = concat (replacements :: [[Replacement]])
      strongestOnly = result' -- strongestReplacements templateEnv result

  SimpleLog.log logHandle LogConstraints $ "about to measure constraint strength; there are " ++ show (length strongestOnly)
  forM_ strongestOnly $ \c -> do
    SimpleLog.log logHandle LogConstraints $ show c
  maybeGoodOnes <- getGoodConstraints dataFilePath env maybeContext m strongestOnly opts logHandle
  SimpleLog.log logHandle LogConstraints $ "maybeGoodOnes: " ++ show maybeGoodOnes
  case maybeGoodOnes of
    -- If getGoodConstraints returns Nothing, it means that no
    -- replacements could be rejected.  In that case, return the whole
    -- set.
    Nothing -> return $ map (\r -> (r, 1.0)) strongestOnly
    Just reps -> return reps


bindSolution :: [Item] -> Bindings -> Bindings
bindSolution sol bs = addAssignmentsToBindings (map toPair sol) bs
  where toPair (AssignI v e) = (v,e)
        toPair _ = undefined

fillInBlanks :: Constraint -> Bindings -> [[Item]] -> [Argument] -> [Argument]
fillInBlanks (Constraint {name="maximum_int_checking"}) bs sols [ Blank, arg1 ] | arg1 /= Blank =
    let maxOfArrayLit (ArrayLit es _dims) = maximum $ map (fromJust . getInteger) es
        maxOfArrayLit _ = undefined
        xsExpression' = argumentToExpression arg1 ^. expRawExpression
        suggestions = map (\sol -> let bs' = bindSolution sol bs
                                       arg1value = expressionToValue bs' xsExpression'
                                       result = maxOfArrayLit arg1value
                                   in result) sols
    in case getUnique suggestions of
         Nothing -> [ Blank, arg1 ]
         Just m -> [ OrdinaryParameter (IntLit m), arg1 ]
fillInBlanks (Constraint {name="gcc"}) bs solutionAssignments [ xs, Blank ] | xs /= Blank =
    let gccOfArrayLit (ArrayLit es _dims) =
          let ints = map (fromJust . getInteger) es
              lower = minimum ints
              upper = maximum ints
          in -- If the lower-to-upper range is too big, give up.
             if upper - lower > 100
             then Nothing
             else Just $ ArrayLit [ makeExp (IntLit c) | v <- [lower..upper], let c = genericLength (filter (==v) ints) ] [(fromIntegral lower,fromIntegral upper)]
        gccOfArrayLit _ = undefined

        xsExpression' = argumentToExpression xs ^. expRawExpression
        suggestions = mapM (\assignments -> let bs' = bindSolution assignments bs
                                                xsvalue = expressionToValue bs' xsExpression'
                                                result = gccOfArrayLit xsvalue
                                            in result) solutionAssignments

        uniqueSuggestion = getUnique <$> suggestions
    in case join uniqueSuggestion of
         Nothing -> [xs, Blank]
         Just x  -> [xs, OrdinaryParameter x]

fillInBlanks _c _bs _sols args = args

-- If the list has only one unique value (all the elements are the
-- same under ==), then return that value.  Otherwise return Nothing.
getUnique :: Eq a => [a] -> Maybe a
getUnique (x:xs) | all (==x) xs = Just x
getUnique _ = Nothing

data ValueFromSolution = SingleValue (Expression')
                       | ManyValues [(Expression')]
  deriving (Show)

-- "expandSolutionIdentifier vids nsols item" takes the list of
-- variable identifiers that appear in the solutions, and converts a
-- given var decl item into a list of var decl items, one per
-- solution.  This transformation is done for "X_used" variables too
-- -- if "x" is in the list, "x_used" will be converted into
-- "x_used_1", "x_used_2", etc.
--
-- Example.  If we have the var decl item:
--
--   array [1..n] of int : x;
--
-- and "x" is in the list of variable identifiers, we convert the
-- declaration item into:
--
--   array [1..n] of int : x_1;
--   array [1..n] of int : x_2;
--   array [1..n] of int : x_3;
--   array [1..n] of int : x_4;
--   array [1..n] of int : x_5;
--
-- (where 5 is the given number of solutions).
expandSolutionIdentifier :: [VarId] -> Int -> Item -> [Item]
expandSolutionIdentifier vids nsols item' =
  case item' of
    VarDeclI (vd@VarDecl { _varDeclIdent = vid, _varDeclExpression = e }) ->
      if vid `elem` vids
      then [ VarDeclI (vd { _varDeclIdent = vid ++ "_" ++ show i}) | i <- [1..nsols] ]
      else fromMaybe [item'] $ do
              base <- stripSuffix "_used" vid
              guard (base `elem` vids)
              return [ VarDeclI (vd { _varDeclIdent = vid ++ "_" ++ show i
                                    , _varDeclExpression = replaceIdent base (base ++ "_" ++ show i) e }) | i <- [1..nsols] ]
    otheritem -> [otheritem]

replaceIdent :: VarId -> VarId -> Maybe (Expression) -> Maybe (Expression)
--replaceIdent before after x | trace (show (before,after,x)) False = undefined
replaceIdent before after x = rewriteBi f x
  where f :: Expression' -> Maybe (Expression')
        f (Ident i) | i == before = Just $ Ident after
        f _ = Nothing

-- Turn a "var" declaration into a "par" declaration.
makePar :: VarDecl -> VarDecl
makePar vd = vd { _varDeclTypeInst = (_varDeclTypeInst vd) { tiInst = Par } }

solve :: String -> Bool -> Model -> GOpts.GlobalizerOptions -> SimpleLog.Handle -> StatisticsIO (SolveResult, [Model])
solve dataFilePath restart m opts logHandle = timeAction "solve" $ do
 let nsols = GOpts.nRandomSolutions opts
 let solveTimeout = fromIntegral $ GOpts.solvingTimeout opts
 let minizincExe = fromMaybe "minizinc" $ GOpts.minizincPath opts
 statisticsFlatZincCall
 statisticsTime "solving via minizinc" $ liftIO $ do
  withSystemTempFile "temp.mzn" $ \mznpath mznhandle -> do
    timeAction "solve/plainShow" $ hPutStrLn mznhandle (plainShow m)
    hClose mznhandle

    withSystemTempFile "output" $ \outpath outhandle -> do
      withSystemTempFile "error" $ \errpath errhandle -> do
        randomSeed <- (randomIO :: IO Word32)
        let gecodeSpecificArgs =
                (if restart && nsols > 0 then "-restart luby" else "") ++ " " ++
                "-r " ++ show randomSeed
        let args =
                [ "--no-optimise"
                , "--solver", "org.gecode.gecode"
                , "--output-mode", "dzn"
                , "-I", dataFilePath
                , "-n", (show nsols)
                , "--fzn-time-limit"
                , show $ solveTimeout
                , "--fzn-flags", gecodeSpecificArgs
                , mznpath]
        (_,_,_,rp) <- createProcess (proc minizincExe args) {std_out = UseHandle outhandle,
                                                             std_err = UseHandle errhandle}
        (ph2,exitCode2) <- timeAction "solve/fzn-gecode" $ flip const ("gecode" :: String) $ do
          SimpleLog.log logHandle LogDebug "starting fzn-gecode"
          SimpleLog.logN logHandle LogHigh $ "G"
          let p = rp
          e <- liftIO $ timeout (solveTimeout) (waitForProcessMsg "waiting for fzn-gecode" p)
          SimpleLog.logN logHandle LogHigh $ "\b"
          SimpleLog.log logHandle LogDebug "fzn-gecode done/killed"
          return (p, e)
        case exitCode2 of
          Just _ -> return ()
          Nothing -> liftIO $ do -- terminateProcess ph2
                                 void $ waitForProcessMsg "waiting for terminated process" ph2
                                 return ()
        output <- do let attemptToRead = do
                           r <- try (BSC.readFile outpath)
                           case r of
                             Right x -> return x
                             Left exc -> do hPutStrLn stderr $ "readFile " ++ outpath ++ " raised: " ++ show (exc :: IOException)
                                            attemptToRead
                     attemptToRead
        errors <- do let attemptToRead = do
                           r <- try (BSC.readFile errpath)
                           case r of
                             Right x -> return x
                             Left exc -> do hPutStrLn stderr $ "readFile " ++ errpath ++ " raised: " ++ show (exc :: IOException)
                                            attemptToRead
                     attemptToRead
        SimpleLog.log logHandle LogSolving "solving stderr:"
        SimpleLog.log logHandle LogSolving $ BSC.unpack errors
        SimpleLog.log logHandle LogSolving "solution output:"
        SimpleLog.log logHandle LogSolving $ BSC.unpack output
        SimpleLog.log logHandle LogSolving "parsing solver output..."
        timeAction "solve/parse" $ case Atto.parseOnly (parseSolverOutput <* Atto.endOfInput) output of
          Right (status, models) -> do
            SimpleLog.log logHandle LogSolving "parsing done"
            return (status, models)
          Left _ -> do
            return (SolveIncomplete, [])

splitSolutions :: String -> (SolveResult, [String])
splitSolutions outputBlob =
    let (st,sols) = loop ([], []) (lines outputBlob)
    in (st, reverse (map (unlines . reverse) sols))
  where
    loop (currentSolution, previousSolutions) [] =
      if null currentSolution
      then (SolveIncomplete, previousSolutions)
      else (SolveIncomplete, currentSolution : previousSolutions)
    loop (currentSolution, previousSolutions) (currentLine:restOfLines) =
      if currentLine == "----------"
      then loop ([], currentSolution : previousSolutions) restOfLines
      else if currentLine == "=========="
           then if null currentSolution
                then (SolveComplete, previousSolutions)
                else (SolveComplete, previousSolutions ++ [currentSolution])
           else if currentLine == "=====UNSATISFIABLE====="
                then (SolveComplete, [])
                else loop (currentLine : currentSolution, previousSolutions) restOfLines

type GroupName = (Integer, (DisjointLocation, Maybe (Expression)))

runGroupsModels :: String -> Bindings -> M.Map GroupName (S.Set (Model), Maybe (Expression)) -> GOpts.GlobalizerOptions -> Maybe String -> ChannelMap -> SimpleLog.Handle -> StatisticsIO [[(Replacement, Double)]]
runGroupsModels dataFilePath env groupMap opts consFilter channelMap logHandle = do
  let selectedGroup = GOpts.selectGroup opts
  let runGroup :: Integer -> (GroupName, (S.Set (Model), Maybe (Expression)))
               -> IO ([(Replacement, Double)], Statistics)
      runGroup groupNumber (ranges,(files, context)) = do
        -- For progress reporting, print the percentage done so far.
        -- This is wrong if the groups are processed in parallel.
        -- let fgroupidx :: Double = fromIntegral groupNumber
        -- let ngroups :: Double = fromIntegral $ length groupMap
        -- do hPrintf stderr "%.2f\n" (100.0 * fgroupidx / ngroups)
        --    hFlush stderr

        SimpleLog.log logHandle LogHigh $ "PROCESSING GROUP " ++ show groupNumber ++ " (" ++ showDisjointLocation "" (fst (snd ranges)) ++ " [ " ++ maybe "" (showExpLocation "") (snd (snd ranges)) ++ " ])"
        SimpleLog.log logHandle LogHigh $ "  this group has " ++ show (S.size files) ++ " models"

        SimpleLog.log logHandle LogModel $ "first model in group:"
        SimpleLog.log logHandle LogModel $ plainShow (head (S.toList files))

        -- Process the group and get the output and statistics.
        (out,st) <- flip runStateT emptyStatistics $ do
                      flip catch (\AbortException -> return ([])) $ do
                        statisticsTime (T.pack ("group " ++ show groupNumber)) $ do
                          processGroupModels dataFilePath env context (S.toList files) opts consFilter channelMap logHandle

        let isTrue ((c,_),_) = name c == "true"
        let out2 = case find isTrue out of
                     Nothing -> out
                     Just x -> [x]

        -- Make sure we force the output.
        _ <- evaluate (length (show (out2, st)))

        SimpleLog.log logHandle LogHigh $ "Group output: " ++ show out2

        return (out2, st)

  forM_ (M.toList groupMap) $ \(_, pair) -> do
    SimpleLog.log logHandle LogDebug $ "new group"
    forM_ (S.toList (fst pair)) $ \m -> do
      SimpleLog.log logHandle LogDebug $ plainShow m
    maybe (return ()) (SimpleLog.log logHandle LogDebug . showExp) (snd pair)

  -- The actions that will run all the groups.
  let actions0 = map (uncurry runGroup) (zip [0..] (M.toList groupMap))
  -- If the user selected a specific group, only run that one.
  let actions = case selectedGroup of
                  Nothing -> actions0
                  Just idx -> [ actions0 !! idx ]

  -- Run all the actions in parallel, gathering the outputs and
  -- statistics.
  (guidoParts, stats) <- unzip <$> liftIO (concurrentlyLimited numCapabilities actions)

  -- Glue the statistics from all the runs together.
  let combinedStats = mconcat stats
  void $ evaluate $ length guidoParts
  mergeStatistics combinedStats
  return guidoParts


-- "strip" a filename
-- e.g. stripped "generated_subproblems/jobshop2x2/m1s0d0.mzn" ==> "jobshop2x2"
stripped :: String -> String
stripped = fst . span (/='/') . tail . snd . break (=='/')



{- From: http://stackoverflow.com/a/22674732 -}
concurrentlyLimited :: Int -> [IO a] -> IO [a]
concurrentlyLimited n tasks = concurrentlyLimited' n (zip [0..] tasks) [] [] (length tasks)

concurrentlyLimited' _ [] [] results _ntasks = do
    hPrintf stdout "%%%%%%mzn-progress 100.00\n"
    hFlush stdout
    return . map snd $ sortBy (comparing fst) results
concurrentlyLimited' 0 todo ongoing results ntasks = do
    (task, newResult) <- waitAny ongoing
    concurrentlyLimited' 1 todo (delete task ongoing) (newResult:results) ntasks
concurrentlyLimited' _ [] ongoing results ntasks = concurrentlyLimited' 0 [] ongoing results ntasks
concurrentlyLimited' n ((i::Int, task):otherTasks) ongoing results ntasks = do
    let ntasks' :: Double = fromIntegral ntasks
    let i' :: Double = fromIntegral i
    hPrintf stdout "%%%%%%mzn-progress %.2f\n" (100.0 * i' / ntasks')
    hFlush stdout
    t <- async $ (i,) <$> task
    concurrentlyLimited' (n-1) otherTasks (t:ongoing) results ntasks


argInt0 :: ArgType
argInt0 = ArgType ArgInt 0
argInt1 :: ArgType
argInt1 = ArgType ArgInt 1
argInt2 :: ArgType
argInt2 = ArgType ArgInt 2
argInt3 :: ArgType
argInt3 = ArgType ArgInt 3

potentialConstraints :: [Constraint]
potentialConstraints = map (uncurry Constraint)
                       [ ("alldifferent",[argInt1])
                       , ("alldifferent_except_0",[argInt1])
                       , ("all_equal_int", [argInt1])
                       , ("atleast", [argInt0, argInt1, argInt0])
                       , ("atmost", [argInt0, argInt1, argInt0])
                       , ("bin_packing", [argInt0, argInt1, argInt1])
                       , ("bin_packing_capa",[argInt1, argInt1, argInt1])
                       , ("bin_packing_load",[argInt1, argInt1, argInt1])
                       , ("bin_packing_load_ub",[argInt1, argInt1, argInt1])
                       , ("binaries_represent_int",[argInt1])
                       , ("binaries_represent_int_3A",[argInt3])
                       , ("binaries_represent_int_3B",[argInt3])
                       , ("binaries_represent_int_3C",[argInt3])
                       , ("channel", [argInt0, argInt1])
                       , ("channelACB", [argInt2, argInt3])
-- --                             , ("circuit",1)  -- needs checking version of constraint
                       , ("circuit_checking", [argInt1])
                       , ("count",[argInt1, argInt0, argInt0])
                       , ("count_geq",[argInt1, argInt0, argInt0])
-- --                             , ("cumulative",4) -- doesn't check its assumptions ("tasks" non-empty)
                       , ("cumulative_assert",[argInt1,argInt1,argInt1,argInt0])
                       , ("decreasing",[argInt1])
                       , ("diffn",[argInt1,argInt1,argInt1,argInt1])
                       , ("distribute",[argInt1,argInt1,argInt1])
                       , ("element", [argInt0,argInt1,argInt0])
--                             , ("exactly", 3) -- "exactly" is the same as "count"
--                              , ("false", [])
                       , ("gcc", [argInt1, argInt1])
                       , ("global_cardinality", [argInt1, argInt1, argInt1])
                       , ("inverse",[argInt1, argInt1])
                       , ("increasing",[argInt1])
-- --                             , ("lex_greater", 2) -- needs checking version
-- --                             , ("lex_greatereq", 2) -- needs checking version
-- --                             , ("lex_less", 2)
-- --                             , ("lex_lesseq", 2)
                       , ("lex_less_int_checking", [argInt1, argInt1])
                       , ("lex_lesseq_int_checking", [argInt1, argInt1])
-- --                             , ("lex2", 1) -- calls lex_lesseq
                       , ("lex2_checking", [argInt2]) -- calls lex_lesseq
--                              -- , ("lineareq", 3)
-- --                             , ("maximum", 2) -- needs checking version
-- --                             , ("minimum", 2) -- needs checking version
                       , ("maximum_int_checking", [argInt0, argInt1])
                       , ("minimum_int_checking", [argInt0, argInt1])
                       , ("member", [argInt1, argInt0])
                       , ("nvalue", [argInt0, argInt1])
                       , ("sliding_sum", [argInt0, argInt0, argInt0, argInt1])
-- --                             , ("sort", 2) -- needs checking version
                       , ("sort_checking", [argInt1, argInt1])
-- --                             , ("strict_lex2", 1) -- calls lex_less
                       , ("strict_lex2_checking", [argInt2])
-- --                             , ("subcircuit", 1) -- needs checking version
                       , ("subcircuit_checking", [argInt1])
-- --                             , ("sum_constraint", 2)
-- --                             , ("value_precede", 3) -- needs checking version
                       , ("true", [])
                       , ("unary", [argInt1,argInt1])
                       , ("value_precede_checking", [argInt0, argInt0, argInt1])
                       ]


findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (x:xs) = do
  b <- f x
  if b then return (Just x)
    else findM f xs


waitForProcessMsg :: String -> ProcessHandle -> IO ExitCode
waitForProcessMsg msg processHandle = do
  waitForProcess processHandle `catch` process_handler
  where process_handler e = do
          hPutStrLn stderr $ "waitForProcessMsg: " ++ msg
          throwM (e :: IOException)
