{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
-- {-# OPTIONS -W -Wall #-}

module GroupOutput where

import Control.Applicative
--import Control.Concurrent.ParallelIO
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM hiding (check)
import Control.DeepSeq
--import Control.DeepSeq.TH
import Control.Monad.Catch
--import Control.Exception hiding (try,catch)
import Control.Exception.Lifted hiding (try,catch)
import Control.Exception.Lens
import Control.Lens hiding (Context, universe)
import Control.Monad.State.Strict
--import Control.Monad.CatchIO
import Control.Monad.Writer (execWriter, tell, Writer, MonadWriter)
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Char8 as BSC
import Data.Data
import Data.Data.Lens
import Data.Foldable (toList)
import Data.Generics.Uniplate.Data
import qualified Data.IntMap as IM
import Data.IORef
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (mempty, Monoid, mconcat)
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
--import Language.MiniZinc.Convert (mznToModel, parseMZN)
--import Language.MiniZinc.LibMzn (typecheckModel)
import Safe hiding (at)
import System.Clock
import System.Environment
import System.Exit
import System.IO
import System.IO.Temp
import System.Process (runProcess, waitForProcess, terminateProcess, ProcessHandle)
import System.Random
import System.Timeout
import Text.Printf
import System.GlobalLock

import System.IO.Unsafe

import Arguments
import Bindings
import Common
import EvalModel
import Loc
import Misc
import MiscExp
import Statistics
import Types
--import Log

import SimpleLog

import Paths_minizinc_globalizer

import Debug.Trace

data AbortException = AbortException
  deriving (Show, Typeable)
instance Exception AbortException

-- -- Should we print debugging information to standard error?
-- shouldLog :: LogCategory -> Bool
-- --shouldLog = (`elem` [LogVariables])
-- shouldLog = (`elem` logList)
-- --              LogSolving
-- --             ])
-- logList =u
--     -- LogChecking :
--     -- -- LogHigh :
--     -- -- LogScoring :
--     LogArgs :
--     -- LogSolving :
--     LogDebug :
--     -- -- LogVariables :
--     []
-- --shouldLog = const False

-- data LogCategory = LogDebug
--                  | LogVariables
--                  | LogArgs
--                  | LogSolving
--                  | LogChecking
--                  | LogHigh
--                  | LogScoring
--   deriving (Eq)

-- logCat :: (MonadIO m) => LogCategory -> String -> m ()
-- logCat cat msg = when (shouldLog cat) $ liftIO $ hPutStr stderr msg >> hFlush stderr
-- logLnCat :: (MonadIO m) => LogCategory -> String -> m ()
-- logLnCat cat msg = when (shouldLog cat) $ liftIO $ hPutStrLn stderr msg >> hFlush stderr
-- recordCat :: LogCategory -> String -> StatisticsIO ()
-- recordCat cat msg = when (True && (True || shouldLog cat)) $ do
--                       key <- use currentKey
-- --                      logTree %= (at key %~ (Just . T.append (T.pack msg) . fromMaybe ""))
--                       logTree %= (at key %~ (Just . (\existing -> T.concat [existing, T.pack "\n", T.pack msg]) . fromMaybe (T.pack "")))

-- record key val = recordLogKey key val

-- logPrintCat :: (MonadIO m, Show a) => LogCategory -> a -> m ()
-- logPrintCat cat x = when (shouldLog cat) $ liftIO $ hPrint stderr x >> hFlush stderr
-- logFlush = liftIO $ hFlush stderr

-- logLn = logLnCat LogDebug
-- logPrint = logPrintCat LogDebug

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
                            ArrayAccess a i <- return $ e'
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
                                                                 
                      
replaceSearch :: Model -> Model
replaceSearch m = modelItems %~ ((++ newSolveItems) . filter (not . isSolveI)) $ m
  where newSolveItems =
            -- We used to use "random" as the variable selection here,
            -- but sometimes it's terrible.  Instead we use a good
            -- variable selection, but keep the random value selection
            -- to encourage diversity in solutions.
            [ SolveI (Annotations [makeExp $ Call "int_default_search" [makeExp $ Call "dom_w_deg" [], makeExp $ Call "indomain_random" []]]) Nothing SolveSatisfy
            , IncludeI "gecode.mzn" Nothing
            ]

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


--processGroupModels :: Bindings -> Maybe (Expression) -> [Model] -> StatisticsIO String
processGroupModels dataFilePath env maybeContext models nRandomSolutions nSampleSolutions solvingTimeout constraintFilter filterArgs maxArgs doImpliesCheck channelMap logHandle = do
--  liftLog logNewGroup
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
--                                             SimpleLog.log $ plainShow m'
                                             SimpleLog.log logHandle LogDebug (plainShow m')
                                             void $ liftIO $ evaluate $ maybe 0 length reps
                                             Just <$> processModelWrapper dataFilePath env maybeContext m' (reps :: Maybe [(Replacement,Double)]) nRandomSolutions nSampleSolutions solvingTimeout constraintFilter filterArgs maxArgs doImpliesCheck channelMap logHandle)
                                          Nothing
                                          (zip [1..] models)
    case result of
      Left NoSolutions -> return []
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
      let nano = timeSpecAsNanoSecs t2 - timeSpecAsNanoSecs t1
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
     -> Int
     -> Integer
     -> SimpleLog.Handle
     -> StateT Statistics IO (Replacement, Double)
timeScoreReplacement dataFilePath env maybeContext m outVars c nSampleSolutions solvingTimeout logHandle = do
  timeAction "scoreReplacement" (scoreReplacement dataFilePath env maybeContext m outVars c nSampleSolutions solvingTimeout logHandle)

scoreReplacement
  :: String
     -> Bindings
     -> Maybe (Expression)
     -> Model
     -> [Expression']
     -> Replacement
     -> Int
     -> Integer
     -> SimpleLog.Handle
     -> StateT Statistics IO (Replacement, Double)
scoreReplacement dataFilePath env maybeContext m outVars c nSampleSolutions solvingTimeout logHandle = do
  let newModel = Model {
               _modelItems =
                 map VarDeclI (topLevelVarDecls m) -- ++ usedVarDecls ++ usedParDecls)
                 ++ annotationItems m
                 ++ filter isFunctionI (m ^. modelItems)
                 ++ instantiate c
                 ++ maybe [] ((:[]) . ConstraintI) maybeContext
                 ++ [SolveI (Annotations [makeExp $ Call "int_default_search" [makeExp $ Call "dom_w_deg" [], makeExp $ Call "indomain_random" []]]) Nothing SolveSatisfy]
                 ++ [OutputI (makeExp $ ArrayLit (map (\v -> makeExp $ Call "show" [v]) (map makeExp $ outVars)) [(1,genericLength outVars)])]
                 ++ [IncludeI "glob.mzn" (Nothing)]
                 ++ [IncludeI "gecode.mzn" (Nothing)]
                 }
  --      csols <- statisticsTime "solve" $ liftIO $ solve True newModel nSampleSolutions
  let evalledNewModel = Language.MiniZinc.evalModelArraySlices newModel

  recordLogKey "new model" (plainShow evalledNewModel)
  --      when (name (fst c) == "bin_packing_capa") $ SimpleLog.log $ "SOLVING FOR CONSTRAINT: " ++ prettyPrintify c
  when (True || name (fst c) == "sliding_sum") $ SimpleLog.log logHandle LogScoring $ "SOLVING FOR CONSTRAINT: " ++ prettyPrintify c
  -- when (True || name (fst c) == "sliding_sum") $ SimpleLog.log logHandle LogScoring $ (plainShow evalledNewModel)
  (solveStatus, csols) <-
    timeAction "scoreReplacement/solve" $
      solve dataFilePath True evalledNewModel nSampleSolutions solvingTimeout logHandle
  --      statisticsFlatZincCall
  -- liftIO $ atomically $ do x <- takeTMVar zincCalls
  --                          putTMVar zincCalls (x+1)
  when (length csols < nSampleSolutions) $ do
  --        SimpleLog.log =<< liftIO (lock (prettyPrintModel newModel))
    SimpleLog.log logHandle LogScoring $ "didn't find enough solutions to constraint (" ++ prettyPrintify c ++ ")"
    SimpleLog.log logHandle LogScoring $ "found only " ++ show (length csols) ++ " solutions using this model:"
    SimpleLog.log logHandle LogScoring $ plainShow evalledNewModel

    -- liftIO $ hPutStrLn stderr $ "didn't find solutions to constraint (" ++ prettyPrintify c ++ ")"
  --      when (True || name (fst c) == "sliding_sum") $ mapM_ (SimpleLog.logPrintCat LogScoring) csols
  results <- 
   timeAction "solveReplacement/checkSols" $
   statisticsTime "forM csols" $ forM (zip [0::Int ..] csols) $ \(solnum, s) -> statisticsTime (T.pack ("solution " ++ show solnum)) $ do
    SimpleLog.log logHandle LogScoring $ plainShow s
    let m' = Model { _modelItems = map VarDeclI (topLevelVarDecls m) -- ++ usedVarDecls ++ usedParDecls)
                                   ++ functionItems m
                                   ++ annotationItems m
                                   ++ solutionToAssignments (s)
                                   ++ [ i | i@(ConstraintI _) <- m ^. modelItems ]
                                   ++ maybe [] ((:[]) . ConstraintI) maybeContext
                   }
  --        when (True || name (fst c) == "sliding_sum") $ SimpleLog.logCat LogScoring . plainShow $ m'
    -- SimpleLog.log logHandle LogDebug $ "Checking satisfiability... "
    -- recordLogKey "solution" (show s)

    SimpleLog.log logHandle LogScoring $ "model to be tested for satisfiability:"
    SimpleLog.log logHandle LogScoring $ plainShow m'

    res <-
      timeAction "solveReplacement/checkSols/modelIsSatisfiable" $
      liftIO $ try (return $! modelIsSatisfiable env m')
    statisticsEvaluation
    -- SimpleLog.logCat LogDebug "done"
    SimpleLog.log logHandle LogScoring $ "satisfies: " ++ (show res)
    let succ' = case res of
                  Left (AssertFailed _) -> False
                  Left (EvalNotPar _) -> False
                  Left (EvalUndefined _) -> False
                  Left (UnfixedVariable _) -> False
                  Left (IndexOutOfRange msg) -> False
  --                          error $ "uh oh: " ++ show (IndexOutOfRange msg)
                  Right b -> b
    return succ'
  let goods = length (filter id results)
  -- liftIO $ atomically $ do x <- takeTMVar evalCalls
  --                          putTMVar evalCalls (x+1)
  statisticsEvaluationAdd (length csols)
  void $ liftIO $ evaluate goods
  --      when (True || name (fst c) == "sliding_sum") $ SimpleLog.logCat LogScoring $ printf "evaluation %d for %s under context %s" goods (show c) (show maybeContext)
  --      when (name (fst c) == "bin_packing_capa") $ recordCat LogScoring $ printf "evaluation %d" goods
  -- liftIO $ printf "num of goods: %d\n" goods
  SimpleLog.log logHandle LogScoring $ "goods: " ++ prettyPrintify c ++ ": " ++ show goods
  return $! (c, fromIntegral goods / fromIntegral nSampleSolutions :: Double)





getGoodConstraints :: String -> Bindings -> Maybe (Expression) -> Model -> [Replacement] -> Int -> Integer -> SimpleLog.Handle -> StatisticsIO (Maybe [(Replacement,Double)])
getGoodConstraints dataFilePath env maybeContext m inter nSampleSolutions solvingTimeout logHandle = do
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
--      SimpleLog.logN logHandle LogHigh "s"
          result <- timeScoreReplacement dataFilePath env maybeContext m outVars c nSampleSolutions solvingTimeout logHandle
          SimpleLog.logN logHandle LogHigh $ "."
          return result
        return $! scoresAndContexts -- (maximumBy compareReplacementAndContext scoresAndContexts)

  let isTrue (c,args) = name c == "true"
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

annotationItems m = [ i | i@(AnnotationI {}) <- m ^. modelItems ]

functionItems m = [ i | i@(FunctionI {}) <- m ^. modelItems ]

compareReplacementAndContext :: (Replacement,Double)
                             -> (Replacement,Double)
                             -> Ordering
compareReplacementAndContext (_r1,score1) (_r2,score2) =
    case compare score1 score2 of
      LT -> LT
      GT -> GT
      EQ -> EQ

-- compareReplacementAndContext :: (Replacement,Context,Double)
--                              -> (Replacement,Context,Double)
--                              -> Ordering
-- compareReplacementAndContext (_r1,ctx1,score1) (_r2,ctx2,score2) =
--     case compare score1 score2 of
--       LT -> LT
--       GT -> GT
--       EQ -> comparing (negate . length) ctx1 ctx2

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
        g [] [] = True
        g (a1:as1) (a2:as2) | a1 == a2 = g as1 as2
                            | otherwise = False
        g _ _ = error "tighter1"

tighter2 :: Bindings -> Replacement -> Replacement -> Bool
-- tighter2 env (Constraint {name="sliding_sum"},[OrdinaryParameter l1,OrdinaryParameter u1,OrdinaryParameter s1,x1]) (Constraint {name="sliding_sum"},[OrdinaryParameter l2,OrdinaryParameter u2,OrdinaryParameter s2,x2]) =
--     ( eqInt env l1 l2 && leInt env u1 u2 )
--     || ( eqInt env u1 u2 && grInt env l1 l2 )
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
  
--instantiate' (FixedExpression e) = e
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
-- <<<<<<< HEAD
    [ ConstraintI (makeExp $ BinOp l BinOpLe u)
    , ConstraintI (makeExp $ BinOp n BinOpLe (makeExp $ Call "length" [xs]))
    , ConstraintI (makeExp $ BinOp n BinOpGr (makeExp $ IntLit 1))
    -- The below constraint is too strict
    -- , ConstraintI (makeExp $ BinOp (makeExp $ BinOp u BinOpLe (makeExp $ BinOp n BinOpMult (makeExp $ Call "ub_array" [xs])))
    --                      BinOpOr
    --                      (makeExp $ BinOp l BinOpGr (makeExp $ BinOp n BinOpMult (makeExp $ Call "lb_array" [xs]))))
    ]
-- =======
--     [ ConstraintI (BinOp l BinOpLe u)
--     , ConstraintI (BinOp n BinOpLe (Call "length" [xs]))
--     , ConstraintI (BinOp n BinOpGr (IntLit 1))
--     -- , ConstraintI (BinOp (BinOp u BinOpLe (BinOp n BinOpMult (Call "ub_array" [xs])))
--     --                      BinOpOr
--     --                      (BinOp l BinOpGr (BinOp n BinOpMult (Call "lb_array" [xs]))))
-- >>>>>>> master
extra "value_precede_checking" [s,t,x] =
    [ ConstraintI (makeExp $ Call "member" [x,s])
    , ConstraintI (makeExp $ Call "member" [x,t]) ]
extra _cname _args = []

notSingleton :: Expression -> [Item]
notSingleton x = [ ConstraintI (makeExp $ BinOp (makeExp $ Call "length" [x]) BinOpGr (makeExp $ IntLit 1)) ]

check :: Bindings -> Int -> String -> [Argument] -> Bool
check bs _nsols "alldifferent" [x] = isVariable x -- && is1DArray bs x
check bs _nsols "alldifferent_except_0" [x] = isVariable x -- && is1DArray bs x
check bs _nsols "all_equal_int" [x] = isVariable x -- && is1DArray bs x
check bs nsols "atleast" [n,x,v] = {-# SCC "insideCheck" #-} isVariable x -- && is1DArray bs x
--                                                  && isInt bs n && isInt bs v
--                                                  && appearsIn bs nsols v x
check bs nsols "atmost" [n,x,v] = isVariable x -- && is1DArray bs x
                                                 && isInt bs n -- && isInt bs v
--                                                 && appearsIn bs nsols v x
check bs _nsols "bin_packing" [c,bin,w] = isNotVariable c -- && isInt bs c
                                                  && isVariable bin -- && is1DArray bs bin
                                                  && isNotVariable w -- && is1DArray bs w
check bs _nsols "bin_packing_capa" [c,bin,w] = isNotVariable c -- && is1DArray bs c
                                                       && isVariable bin -- && is1DArray bs bin
                                                       && isNotVariable w -- && is1DArray bs w
check bs _nsols "bin_packing_load" [l,bin,w] = isVariable l -- && is1DArray bs l
                                                       && isVariable bin -- && is1DArray bs bin
                                                       && isNotVariable w -- && is1DArray bs w
check bs _nsols "bin_packing_load_ub" [l,bin,w] = isVariable l -- && is1DArray bs l
                                                         && isVariable bin -- && is1DArray bs bin
                                                         && isNotVariable w -- && is1DArray bs w
check bs _nsols "binaries_represent_int" [b] = isVariable b
check bs _nsols "binaries_represent_int_3A" [b] = isVariable b
check bs _nsols "binaries_represent_int_3B" [b] = isVariable b
check bs _nsols "binaries_represent_int_3C" [b] = isVariable b
check bs _nsols "channel" [x,a] =
--    is1DArray bs a && isInt bs x
    (isVariable a || isVariable x)
check bs _nsols "channelACB" [x,a] =
--    is1DArray bs a && isInt bs x
    (isVariable a || isVariable x)
check bs _nsols "circuit_checking" [x] = isVariable x -- && is1DArray bs x
check bs _nsols "count" [x,y,c] = isVariable x -- && is1DArray bs x
                                          -- && isInt bs y
                                          -- && isInt bs c
check bs _nsols "count_geq" [x,y,c] =
  -- FORBIDDEN
  False && isVariable x
check bs _nsols "cumulative" [s,d,r,b] =
    -- is1DArray bs s
    -- && is1DArray bs d
    -- && is1DArray bs r
    -- && isInt bs b
    (isVariable s || isVariable d || isVariable r)
    && s /= d && d /= r && s /= r
check bs nsols "cumulative_assert" args = check bs nsols "cumulative" args
check bs _nsols "decreasing" [x] = isVariable x -- && is1DArray bs x
check bs _nsols "diffn" [x,y,dx,dy] =
    -- is1DArray bs x
    -- && is1DArray bs y
    -- && is1DArray bs dx
    -- && is1DArray bs dy
      ((isVariable x && isVariable y) || (isVariable dx && isVariable dy))
    && x /= dx && x /= dy && y /= dx && y /= dy
    && x /= y
check bs _nsols "distribute" [c,v,b] = True 
                                       -- && is1DArray bs c
                                       --         && is1DArray bs v
                                       --         && is1DArray bs b
check bs _nsols "element" [i,a,v] =
  -- FORBIDDEN
  False && True -- && isInt bs i && isInt bs v
                                         --    && is1DArray bs a
check bs _nsols "exactly" [n,x,v] = isNotVariable n -- && isInt bs n
                                            && isVariable x -- && is1DArray bs x
                                            && isNotVariable v -- && isInt bs v
check bs _nsols "gcc" [x,cs] = is1DArray bs x -- && is1DArray bs cs
check bs _nsols "global_cardinality" [x,cv,cs] = -- is1DArray bs x
                                                         isNotVariable cv -- && is1DArray bs cv
                                                         -- && is1DArray bs cs
check bs _nsols "increasing" [x] = isVariable x -- && is1DArray bs x
check bs _nsols "inverse" [x,y] = True -- && is1DArray bs x && is1DArray bs y
check bs _nsols "lex_less_int_checking" [x,y] = True -- && is1DArray bs x && is1DArray bs y
check bs _nsols "lex_lesseq_int_checking" [x,y] = x /= y -- && is1DArray bs x && is1DArray bs y
check bs _nsols "lex2" [x] = isVariable x -- && is2DArray bs x
check bs nsols "lex2_checking" [x] = check bs nsols "lex2" [x]
check bs _nsols "lineareq" [x,y,n] = True -- && is1DArray bs x && is1DArray bs y
                                          --    && isInt bs n
check bs _nsols "member" [xs,x] =
  -- FORBIDDEN
  False && True -- && is1DArray bs xs && isInt bs x
check bs _nsols "maximum_int_checking" [x,xs] = True -- && is1DArray bs xs && isInt bs x
check bs _nsols "minimum_int_checking" [x,xs] = True -- && is1DArray bs xs && isInt bs x
check bs _nsols "nvalue" [n,x] = True -- && is1DArray bs x && isInt bs n
check bs _nsols "sliding_sum" [l,u,s,vs] = isNotVariable l -- && isInt bs l
                                                   && isNotVariable u -- && isInt bs u
                                                   && isNotVariable s -- && isInt bs s
                                                   && (l /= u) && (l /= s) && (u /= s)
                                                   && isVariable vs -- && is1DArray bs vs
check bs _nsols "sort_checking" [x,y] = -- is1DArray bs x && is1DArray bs y
                                                 (isVariable x || isVariable y)
check bs nsols "strict_lex2" [x] = check bs nsols "lex2" [x]
check bs nsols "strict_lex2_checking" [x] = check bs nsols "strict_lex2" [x]
check bs _nsols "subcircuit_checking" [x] = isVariable x -- && is1DArray bs x
--check bs nsols (Constraint "sum_constraint" 2) [n,x] | trace (show (n,x)) False = undefined
check bs _nsols "sum_constraint" [n,x] = isVariable x -- && is1DArray bs x
                                      -- && isInt bs n
check bs _nsols "unary" [s,d] =
    -- is1DArray bs s
    -- && is1DArray bs d
     (isVariable s || isVariable d)
check bs nsols "value_precede" [s,t,x] = isNotVariable s -- && isInt bs s
                                                  && isNotVariable t -- && isInt bs t
                                                  && isVariable x -- && is1DArray bs x
--                                                  && (appearsIn bs nsols s x && appearsIn bs nsols t  x)
                                                  && s /= t
check bs nsols "value_precede_checking" args = check bs nsols "value_precede" args
check _bs _nsols _cname _args = True

isVariable :: Argument -> Bool
isVariable (ErstwhileVariable {}) = True
isVariable (ArgumentArrayAccess a idx) = isVariable a
isVariable _ = False
isNotVariable :: Argument -> Bool
isNotVariable = not . isVariable

-- This needs to be fixed: an array access "x[p,_]" doesn't have an identifier.
--
-- What we need instead is something of the type "Argument -> TypeInst"
-- getSolutionIdentifier :: Argument -> Int -> Maybe String
-- getSolutionIdentifier (ErstwhileVariable vid) n = Just $ vid ++ "_" ++ show n
-- getSolutionIdentifier x                       _ = getIdentifier x

-- argumentTypeInst :: Bindings -> Argument -> Maybe TypeInst
-- argumentTypeInst (OrdinaryParameter e') = 

-- appearsIn :: Bindings -> Int -> Argument -> Argument -> Bool
-- appearsIn env nsols x xs =
--   case (getArgumentValues env nsols x, getArgumentValues env nsols xs) of
--     (Just (SingleValue x'), Just (SingleValue (ArrayLit es _))) -> x' `elem` (map (view expRawExpression) es)
--     (Just (SingleValue x'), Just (ManyValues es)) -> x' `elem` [ IntLit i | e <- es, IntLit i <- universe e ]
--     _ -> False


-- Make sure at least one argument is a variable.
argCheck :: [Argument] -> Bool
argCheck args = any isVariable args

noBlanks :: [Argument] -> Bool
noBlanks = not . any blank
  where blank Blank = True
        blank _     = False

-- nRandomSolutions :: Num a => a
-- nRandomSolutions = 30

-- nSampleSolutions :: Num a => a
-- nSampleSolutions = 30

evalModelIdentifiers m =
  snd $ runResolve $ do
    me <- resolveModel m
    (vdidmap, _bmap) <- get
    let f :: Expression -> Expression
        f e@(expRaw -> Bound x (VarDeclId vdid)) =
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
      es = [ e | ConstraintI e <- m ^. modelItems ]
      vars = concatMap variablesDefinedByItem (m ^. modelItems)
  in nub vars

variablesDefinedByItem :: Item -> [Expression']
variablesDefinedByItem (VarDeclI vd) = do
  let ti = vd ^. varDeclTypeInst
      ident = vd ^. varDeclIdent
  guard (tiInst ti == Var)
  return $ Ident ident
  -- case tiRanges ti of
  --   OrdinaryRanges [] -> [Ident ident]
  --   OrdinaryRanges tis -> do idx <- mapM chooseIndex tis
  --                            return $ ArrayAccess (makeExp $ Ident ident) idx
variablesDefinedByItem _ = []

chooseIndex ti =
  case tiDomain ti of
    Just e -> case e ^. expRawExpression of
                SetLit (SLVIntSetVal [(l,u)]) -> map (makeExp . IntLit) [l..u]
                BinOp le BinOpDotdot ue ->
                    let IntLit l = le ^. expRawExpression
                        IntLit u = ue ^. expRawExpression
                    in map (makeExp . IntLit) [l..u]
                x -> error $ "chooseIndex: " ++ show x

-- This is a specialised version of "sequence", but has better memory
-- behaviour.  (For more information see
-- http://www.cmears.id.au/articles/sequence-space-leak.html)
cartesianProduct1 :: [[a]] -> [[a]]
cartesianProduct1 = cartesianProduct' [] . reverse
  where cartesianProduct' :: [a] -> [[a]] -> [[a]]
        cartesianProduct' acc [] = [acc]
        cartesianProduct' acc (xs:xss) =
            concatMap (\x -> cartesianProduct' (x:acc) xss) xs

-- -- "usedVarDecl" takes a group of variables (which have the same
-- -- identifier "X") and produces the "X_used" declaration.
-- usedVarDecl :: Model -> [Expression'] -> Maybe (VarDecl ())
-- usedVarDecl m [v] =
--   let vid = groupIdentifier [v]
--       vd = fromMaybe (error "no vardecl") (findVarDecl m vid)
--       (TypeInst ti bt _rs st d) = vd ^. varDeclTypeInst
--   in do guard (st == Plain)
--         return $
--           VarDecl { _varDeclIdent = vid ++ "_used"
--                   , _varDeclAnnotations = mempty
--                   , _varDeclDecoration = mempty
--                   , _varDeclTypeInst = TypeInst ti bt (OrdinaryRanges []) st d
--                   , _varDeclExpression = Just . makeExp $ v }
-- usedVarDecl m g =
--     let vid = groupIdentifier g
--         vd = fromMaybe (error "no vardecl") (findVarDecl m vid)
--         (TypeInst ti bt _rs st d) = vd ^. varDeclTypeInst
--     in do guard (st == Plain)
--           return $
--             VarDecl { _varDeclIdent = vid ++ "_used"
--                     , _varDeclDecoration = mempty
--                     , _varDeclAnnotations = mempty
--                     , _varDeclTypeInst =
--                         TypeInst { tiInst = ti
--                                  , tiBase = bt
--                                  , tiSet = st
--                                  , tiDomain = d
--                                  , tiRanges = OrdinaryRanges
--                                    [parInt { tiDomain =
--                                                (Just (makeExp $ BinOp (makeExp $ IntLit 1)
--                                                             BinOpDotdot
--                                                             (makeExp $ IntLit (genericLength g)))) }] }
                                               
--                     , _varDeclExpression = Just (makeExp $ ArrayLit (map makeExp g) [(1,genericLength g)]) }


-- Does the model have exactly two constraints, and does one
-- constraint seem to imply the other?
impliesCheck :: String -> Bindings -> Model -> Int -> Integer -> SimpleLog.Handle -> StatisticsIO Bool
impliesCheck dataFilePath env m nRandomSolutions solvingTimeout logHandle = do
    SimpleLog.log logHandle LogSolving "Starting implies check"
    result <-
      case filter isConstraintI (m ^. modelItems) of
        [c1,c2] -> do let m' = m & modelItems %~ filter (not . isConstraintI)
                      i1 <- impliesCheck2 dataFilePath env m' c1 c2 nRandomSolutions solvingTimeout logHandle
                      if i1 then return True
                        else impliesCheck2 dataFilePath env m' c2 c1 nRandomSolutions solvingTimeout logHandle
        _ -> return False
    SimpleLog.log logHandle LogSolving $ "Implies check returns: " ++ show result
    return result

-- See if C1 implies C2 in the context of model M (which has no other
-- constraints).
impliesCheck2 :: String -> Bindings -> Model -> Item -> Item -> Int -> Integer -> SimpleLog.Handle -> StatisticsIO Bool
impliesCheck2 dataFilePath env m c1 c2 nRandomSolutions solvingTimeout logHandle = do
    -- Find some solutions to M+C1.
    SimpleLog.log logHandle LogDebug "impliesCheck2"
    (solveStatus, sols1) <-
      timeAction "impliesCheck2/solve" $
      solve dataFilePath True (m & modelItems %~ (c1:)) nRandomSolutions solvingTimeout logHandle
    if length sols1 < nRandomSolutions
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
                                Left (IndexOutOfRange msg) -> False
--                                    error $ "uh oh: " ++ show (IndexOutOfRange msg)
                                Right b -> b

              if succ' then f ss else return False
        f (sols1)

-- Convert a solution --- a list of output lines --- into a list of
-- assign items.
-- solutionToAssignments :: String -> [ Item ]
-- solutionToAssignments = _modelItems . generaliseDecoration . fromRight . flip parseModel "input" . BSC.pack
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

-- freeIdentifiers :: forall a. (Typeable a, Data a) => Expression -> [VarId]
-- freeIdentifiers expr = execWriter $ descendBiM (f emptyBindings) expr
--   where
--     handleComprehension env (Comprehension body gens whereExp _compType) = do
--        newgens <- descendBiM (f env) gens
--        let newenv = flip addVarDeclsToBindings env $
--                     [ uninitialise vd | (Generator {_genVarDecls=vds}) <- newgens, vd <- vds ]
--        void $ f newenv body
-- --                                  descendBiM (f newenv) gens
--        void $ descendBiM (f newenv) whereExp
--        return $ expr
--     f :: Bindings -> Expression -> Writer [VarId] (Expression)
--     f env e =
--         case _expRawExpression e of
--           (Let vds body) -> f (addVarDeclsToBindings (map id vds) env) body
--           (GenCall _p c) -> handleComprehension env c
--           (ComprehensionExpr c) -> handleComprehension env c
--           (Ident y)      -> do
--             case lookupTypeInst y env of
--               Nothing -> tell [y]
--               Just _ -> return ()
--             return $ e
--           y              -> descendBiM (f env) y >> return e

-- Delete a VarDecl's initialisation expression
uninitialise :: VarDecl -> VarDecl
-- uninitialise (VarDecl {_varDeclTypeInst=ti
--                       ,_varDeclIdent=ident}) = VarDecl ti ident Nothing mempty mempty
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
                    -> Int
                    -> Int
                    -> Integer
                    -> Maybe String
                    -> Bool
                    -> Int
                    -> Bool
                    -> ChannelMap
                    -> SimpleLog.Handle
                    -> StatisticsIO [(Replacement,Double)]
processModelWrapper dataFilePath env maybeContext m maybeReps nRandomSolutions nSampleSolutions solvingTimeout constraintFilter filterArgs maxArgs doImpliesCheck channelMap logHandle = do
  let innerHandler action = 
          catching failedProcessReason action $ \fpr -> do
            -- case fpr of
            -- --   NoSolutions -> liftIO $ hPutStrLn stderr $ "(no solutions)" -- \nin this submodel:\n" ++ plainShow m
            --   ImpliesCheck -> liftIO $ hPutStrLn stderr $ "(implies check succeeded)" -- on this submodel:\n" ++ plainShow m ++ "\nwith this context: " ++ maybe "(no context)" showExp maybeContext
            -- --   TooBig -> liftIO $ hPutStrLn stderr $ "(too big)"
            --   _ -> return ()
            return []
      outerHandler :: StatisticsIO [(Replacement,Double)] -> StatisticsIO [(Replacement,Double)]
      outerHandler action = catch action $ \err -> do
                              liftIO $ hPutStrLn stderr $ "processModelWrapper caught: " ++ show (err::SomeException)
                                             ++ "\nin this model:\n" ++ plainShow m
                              liftIO $ exitFailure
                              return []
  outerHandler $ innerHandler $ processModel dataFilePath env maybeContext m maybeReps nRandomSolutions nSampleSolutions solvingTimeout constraintFilter filterArgs maxArgs doImpliesCheck channelMap logHandle
  -- result <- trying _ErrorCall (processModel m maybeReps)
  -- case result of
  --   Left err -> error $ "processModelWrapper caught: " ++ err
  --   Right x -> return x
  -- catch 
        
  -- catching failedProcessReason (processModel env m maybeReps) $ \fpr -> do
  --     case fpr of
  --       NoSolutions -> liftIO $ hPutStrLn stderr $ "no solutions\nin this submodel:\n" ++ plainShow m
  --       ImpliesCheck -> liftIO $ hPutStrLn stderr $ "(implies check succeeded)"
  --     return []


-- Prepare a model to have its solutions sampled.
prepareModel :: String -> Bindings -> Model -> Int -> Integer -> Bool -> SimpleLog.Handle -> StatisticsIO (Model)
prepareModel dataFilePath env m nRandomSolutions solvingTimeout doImpliesCheck logHandle = do
  -- Alter the search annotation and output item.
  let modelToSolve = replaceOutput . replaceSearch $ m

  -- If the model has only two constraints and one implies the other,
  -- give up.
  statisticsTime "implies check" $ do
    implies <-
        if doImpliesCheck
        then impliesCheck dataFilePath env modelToSolve nRandomSolutions solvingTimeout logHandle
        else return False
    when implies $ do
      SimpleLog.log logHandle LogSolving $ "implies check succeeded on this model: " ++ plainShow modelToSolve
      throwingM failedProcessReason ImpliesCheck
  return modelToSolve


processModel :: String
             -> Bindings
             -> Maybe (Expression)
             -> Model
             -> Maybe [(Replacement,Double)]
             -> Int
             -> Int
             -> Integer
             -> Maybe String
             -> Bool
             -> Int
             -> Bool
             -> ChannelMap
             -> SimpleLog.Handle
             -> StatisticsIO [(Replacement,Double)]
processModel dataFilePath env maybeContext m' maybeReps nRandomSolutions nSampleSolutions solvingTimeout constraintFilter filterArgs maxArgs doImpliesCheck channelMap logHandle = do
  -- Add the context to the model.
  let m = Language.MiniZinc.evalModelArraySlices (m' & modelItems %~ (++ (maybe [] ((:[]) . ConstraintI) maybeContext)))

  SimpleLog.log logHandle LogHigh $ "about to solve model"

  modelToSolve <- prepareModel dataFilePath env m nRandomSolutions solvingTimeout doImpliesCheck logHandle

  (result, solutions) <- statisticsTime "solving model" $ do
    SimpleLog.log logHandle LogSolving $ "solving model..."
    SimpleLog.log logHandle LogSolving $ plainShow modelToSolve
    -- Find some random solutions.
    (result, solutions') <-
      timeAction "processModel/solve" $
      solve dataFilePath True modelToSolve nRandomSolutions solvingTimeout logHandle
    -- mapM_ (SimpleLog.logPrintCat LogSolving) solutions'
    SimpleLog.log logHandle LogSolving "done solving model, counting solutions..."
    void $ liftIO $ evaluate $ length solutions'
    SimpleLog.log logHandle LogSolving $ "there are " ++ show (length solutions') ++ " solutions (" ++ show result ++ ")"
    return (result, solutions')
  let solutionAssignments = map solutionToAssignments solutions
  forM_ solutions $ \sol -> do
    SimpleLog.log logHandle LogSolving $ "solution: " ++ plainShow sol

  when (result == SolveIncomplete && (nRandomSolutions == 0 || length solutions < nRandomSolutions)) $ do
    SimpleLog.log logHandle LogSolving $ "not enough solutions to this model"
    SimpleLog.log logHandle LogSolving $ "(found only " ++ show (length solutions) ++ ")"
    -- liftIO $ hPutStrLn stderr $ plainShow modelToSolve
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
      firstWord l = headNote "solution line has no head?" (words l)
  let identifiersInSolution = --map firstWord (lines (firstSolution))
        map (\(AssignI x v) -> x) (_modelItems firstSolution)
            
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
  potentialArguments <- computePotentialArguments templateEnv identifiersInSolution parVarDecls allConstraintsExpression filterArgs channelMap logHandle

  let baseEnv = topLevelBindings m

  let typeInstToArgType ti =
          case ti of
               TypeInst { tiRanges = OrdinaryRanges rs, tiBase = BTInt, tiSet = Plain } -> ArgType ArgInt (genericLength rs)
               TypeInst { tiRanges = OrdinaryRanges rs, tiBase = BTInt, tiSet = Set } -> ArgType ArgSetInt (genericLength rs)
               TypeInst { tiRanges = OrdinaryRanges rs, tiBase = BTUnknown,
                          tiDomain = Just (expRaw -> Ident i) } ->
                            fromMaybe (error ("couldn't look up \"" ++ i ++ "\"")) $ do
                              vd <- lookupVarDecl i baseEnv
                              let ti' = vd ^. varDeclTypeInst
--                              return $! trace (i ++ " has typeinst " ++ show ti') ()
                              let ArgType ArgSetInt _ = typeInstToArgType ti'
                              return $ ArgType ArgInt (genericLength rs)
               ti -> error $ "getArgTypeIdent: unknown typeinst: " ++ show ti

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
--  SimpleLog.log "usedVarDecls"
--  mapM_ (SimpleLog.log . showVarDecl) usedVarDecls
  SimpleLog.log logHandle LogArgs "model:"
  SimpleLog.log logHandle LogArgs (plainShow m)
  SimpleLog.log logHandle LogArgs "context:"
  SimpleLog.log logHandle LogArgs (maybe "<no context>" showExp maybeContext)
  SimpleLog.log logHandle LogArgs "potentialArguments"
  let showArg Blank = "<blank>"
      showArg a = show (getArgType a) ++ ": " ++ show a
  mapM_ (SimpleLog.log logHandle LogArgs . showArg) potentialArguments
--   SimpleLog.log logHandle LogArgs "potentialArguments1Filtered"
--   let showArg Blank = "<blank>"
--       showArg a = show (getArgType a) ++ ": " ++ show a
-- --  mapM_ (SimpleLog.log logHandle LogArgs . showArg) potentialArguments1Filtered
  SimpleLog.log logHandle LogArgs "(end of potential args)"

  let argumentsByType = M.fromListWith (++) [ (t,[a]) | a <- potentialArguments, let mt = getArgType a, isJust mt, let Just t = mt ]
  -- SimpleLog.logPrintCat LogArgs argumentsByType
  -- SimpleLog.log "potential arguments" $ intercalate "\n" (map showArg potentialArguments)
  -- recordCat LogArgs $ intercalate " " (map showArg potentialArguments1)
  -- recordCat LogArgs $ intercalate " " (map showArg potentialArguments1Filtered)

  let acceptableConstraint (Constraint name args) =
          and [ name /= "atmost"
              , name /= "atleast"
              , case constraintFilter of
                  Nothing -> True
                  Just s -> s `isInfixOf` name
              ]
  -- name == "bin_packing_capa"

  let constraintsToConsider = filter acceptableConstraint potentialConstraints
  
  SimpleLog.log logHandle LogHigh $ "considering " ++ show (length constraintsToConsider) ++ " constraints"

  replacements <- forM constraintsToConsider $ \c -> statisticsTime (T.pack (show c)) $ do
   flip (catching failedProcessReason) (\fpr -> do case fpr of
                                                     TooBig -> -- liftIO (hPutStrLn stderr ("(too big - " ++ show (name c) ++ ")")) >>
                                                                 return []
                                                     otherReason -> throwingM failedProcessReason otherReason) $ do
--    liftIO $ hPutStrLn stderr $ "calculating cartesian product for " ++ show c
    SimpleLog.log logHandle LogConstraints $ "considering constraint " ++ name c
--    let cart = (cartesianProduct1 (replicate (length (argtypes c)) potentialArguments))
    let chooseArgument t = Blank : M.findWithDefault [] t argumentsByType
    let cart = mapM chooseArgument (argtypes c)
        numArgLists = length cart
    SimpleLog.log logHandle LogArgs $ "for constraint " ++ (name c) ++ " there are " ++ show numArgLists ++ " argument lists"
    SimpleLog.log logHandle LogArgs $ "  " ++ take 100 (show cart)

    when (numArgLists > maxArgs) $ do
      throwingM failedProcessReason TooBig
    
--    liftIO $ hPutStrLn stderr $ "done (" ++ show (length cart) ++ ")"
    let rawargslist = cart
    evalCalls <- liftIO $ newTMVarIO (0::Int)
    replacements' <- statisticsTime "argumentlists" $ forM rawargslist $ \rawargs' -> statisticsTime (T.pack (show rawargs')) $ do
      -- when (True || name c == "maximum_int_checking") $
      
      -- SimpleLog.log logHandle LogArgs $ "considering " ++ show rawargs'
      let rawargs = rawargs'
      -- SimpleLog.log logHandle LogArgs $ "filling in blanks... " ++ show rawargs'
      eitherArgs <- liftIO $ try $ return $! fillInBlanks c templateEnv solutionAssignments rawargs
      let args = case eitherArgs of
                   Left (IndexOutOfRange msg) -> rawargs
                   Left e -> rawargs
                   Right r -> r
      -- SimpleLog.log logHandle LogArgs $ "blanks filled: " ++ show rawargs'
          --args = rawargs
      -- when (name c == "maximum_int_checking") $
      --   liftIO $ hPutStrLn stderr $ "considering " ++ show args
      let typecheckResult = typecheck c args
      let checkResult = {-# SCC "insideCheckResult" #-} check templateEnv (length solutions) (name c) args
      -- when (True || name c == "maximum_int_checking") $
      recordLogKey "check result" (show (checkResult))
      -- SimpleLog.log logHandle LogArgs $ "checkResult: " ++ show checkResult
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
          -- when (name c == "maximum_int_checking") $
          --   liftIO $ hPutStrLn stderr $ "CONSIDERING " ++ show rawargs'
          let extraCon = concat [ (extra (name c)) args'
                                 | -- i <- [1..length solutions],
                                   let args' = map argumentToExpression args ]
              coreCon = concat [ (core c) args'
                                 | -- i <- [1..length solutions],
                                   let args' = map argumentToExpression args ]
          let items = extraCon ++ coreCon
              repItem = headNote "core items list empty?" coreCon
          let modelToCheck = constraintTemplateModel'' & modelItems <>~ items
          recordLogKey "model to check" (plainShow modelToCheck)
          when (True || name c == "bin_packing_capa") $ SimpleLog.log logHandle LogChecking $ "CHECKING FOR: " ++ prettyPrintify (c,args) ++ " (there are " ++ show (length solutionAssignments) ++ " to check)"
          when (True || name c == "maximum_int_checking") $ SimpleLog.log logHandle LogChecking $ plainShow modelToCheck
          -- logFlush
--          findResult <- liftIO $ try (return $! findUnsatisfiableConstraint env modelToCheck)

          -- Try to find an assignment that makes the model unsatisfiable.
          findResult <- try $ findM (\assigns -> do
                                                          statisticsEvaluation
                                                          return $ not (modelIsSatisfiable' env (modelToCheck & modelItems <>~ assigns))) solutionAssignments
--          recordLogKey "find result" (show findResult)
          -- let unsatConstraint = case findResult of
          --                         Right (Just c) -> Just c
          --                         _ -> Nothing
--          let result2 = isJust unSatCon
--          void $ liftIO $ evaluate result2
          liftIO $ atomically $ do x <- takeTMVar evalCalls
                                   putTMVar evalCalls (x+1)
          when (True || name c == "bin_packing_capa") $ SimpleLog.log logHandle LogChecking $
            case findResult of
              Left _ -> show findResult
              Right Nothing -> show findResult
              Right (Just items) -> "Right Just " ++ plainShow (Model items)
          let success2 = case findResult of
                           Left (AssertFailed _) -> False
                           Left (EvalNotPar _) -> False
                           Left (EvalUndefined _) -> False
                           Left (UnfixedVariable _) -> False
                           Left (IndexOutOfRange msg) ->
                               if null [ () | OrdinaryParameter (ArrayAccess _ _) <- args ]
                                  && null [ () | ArgumentArrayAccess {} <- args ]
                               then False -- error $ "uh oh: " ++ show (IndexOutOfRange msg)
                               else False
--                           Right mb -> not (isJust mb)
                           -- We succeed if there is no such violating assignment.
                           Right mb -> isNothing mb
          when (True || name c == "bin_packing_capa") $ SimpleLog.log logHandle LogChecking $ show success2 ++ "\t      " ++ prettyPrintify (c,args)
--          when (name c == "sliding_sum") $ SimpleLog.logPrintCat LogChecking unsatConstraint
--          liftLog $ logRecordReplacement (c,args) modelToCheck success2 unsatConstraint -- somehow record the failed constraint
          return $ if success2 then Just (c, args) else Nothing
        else return Nothing
    liftIO (atomically (takeTMVar evalCalls)) >>= statisticsEvaluationAdd
    return (catMaybes replacements')

  let result = concat (replacements :: [[Replacement]])
      strongestOnly = result -- strongestReplacements templateEnv result

  SimpleLog.log logHandle LogConstraints $ "about to measure constraint strength; there are " ++ show (length strongestOnly)
  forM_ strongestOnly $ \c -> do
    SimpleLog.log logHandle LogConstraints $ show c
  maybeGoodOnes <- getGoodConstraints dataFilePath env maybeContext m strongestOnly nSampleSolutions solvingTimeout logHandle
  SimpleLog.log logHandle LogConstraints $ "maybeGoodOnes: " ++ show maybeGoodOnes
  case maybeGoodOnes of
    -- If getGoodConstraints returns Nothing, it means that no
    -- replacements could be rejected.  In that case, return the whole
    -- set.
    Nothing -> return $ map (\r -> (r, 1.0)) strongestOnly
    Just reps -> return reps
  
  -- SimpleLog.log logHandle LogHigh $ "End of processModel; here are the surviving constraints:"
  -- mapM_ (SimpleLog.logPrint logHandle LogHigh) goodOnes
  -- SimpleLog.log logHandle LogHigh "(done)"

--  return (goodOnes :: [(Replacement,Double)])


bindSolution :: [Item] -> Bindings -> Bindings
bindSolution sol bs = addAssignmentsToBindings (map toPair sol) bs
  where toPair (AssignI v e) = (v,e)

fillInBlanks :: Constraint -> Bindings -> [[Item]] -> [Argument] -> [Argument]
fillInBlanks (Constraint {name="maximum_int_checking"}) bs sols [ Blank, arg1 ] | arg1 /= Blank =
    let maxOfArrayLit (ArrayLit es _dims) = maximum $ map (fromJust . getInteger) es
        xsExpression' = argumentToExpression arg1 ^. expRawExpression
        suggestions = map (\sol -> let bs' = bindSolution sol bs
                                       arg1value = expressionToValue bs' xsExpression'
                                       result = maxOfArrayLit arg1value
                                   in result) sols
    in case getUnique suggestions of
         Nothing -> [ Blank, arg1 ]
         Just m -> [ OrdinaryParameter (IntLit m), arg1 ]
-- --fillInBlanks (Constraint {name="sum_constraint"}) bs nsols [ Blank, arg1 ] | trace ("fillInBlanks " ++ show (Blank,arg1)) False = undefined
-- fillInBlanks (Constraint {name="sum_constraint"}) bs nsols [ Blank, arg1 ] =
--     let sumOfArrayLit (ArrayLit es _dims) = sum <$> mapM getInt es
--         sumOfArrayLit _e                  = Nothing
--         getSum :: ValueFromSolution -> Maybe Int
--         getSum (SingleValue e) = sumOfArrayLit e
--         getSum (ManyValues es) = do sums <- mapM sumOfArrayLit es
--                                     case nub sums of
--                                       [x] -> Just x
--                                       _ -> Nothing
--         value :: Maybe Int
--         value = getSum =<< getArgumentValues bs nsols arg1
--     in case value of
--          Nothing -> [Blank, arg1]
--          Just x  -> [OrdinaryParameter (IntLit x), arg1]
fillInBlanks (Constraint {name="gcc"}) bs solutionAssignments [ xs, Blank ] | xs /= Blank =
    let gccOfArrayLit (ArrayLit es _dims) =
          let ints = map (fromJust . getInteger) es
              lower = minimum ints
              upper = maximum ints
          in -- If the lower-to-upper range is too big, give up.
             if upper - lower > 100
             then Nothing
             else Just $ ArrayLit [ makeExp (IntLit c) | v <- [lower..upper], let c = genericLength (filter (==v) ints) ] [(fromIntegral lower,fromIntegral upper)]

        xsExpression' = argumentToExpression xs ^. expRawExpression
        suggestions = mapM (\assignments -> let bs' = bindSolution assignments bs
                                                xsvalue = expressionToValue bs' xsExpression'
                                                result = gccOfArrayLit xsvalue
                                            in result) solutionAssignments

        uniqueSuggestion = getUnique <$> suggestions
    in case join uniqueSuggestion of
         Nothing -> [xs, Blank]
         Just x  -> [xs, OrdinaryParameter x]

    --     getGcc :: ValueFromSolution -> Maybe (Expression)
    --     getGcc (SingleValue e) = makeExp <$> gccOfArrayLit e
    --     getGcc (ManyValues es) = do gccs <- mapM gccOfArrayLit es
    --                                 case nub gccs of
    --                                   [x] -> Just (makeExp x)
    --                                   _ -> Nothing
    --     -- value :: Maybe (Expression)
    --     -- value = getGcc =<< map getArgumentValues bs nsols xs
    -- in undefined -- case value of
    --    --   Nothing -> [xs, Blank]
    --    --   Just x  -> [xs, OrdinaryParameter (x ^. expRawExpression)]
fillInBlanks _c _bs _sols args = args

-- If the list has only one unique value (all the elements are the
-- same under ==), then return that value.  Otherwise return Nothing.
getUnique :: Eq a => [a] -> Maybe a
getUnique (x:xs) | all (==x) xs = Just x
getUnique _ = Nothing

data ValueFromSolution = SingleValue (Expression')
                       | ManyValues [(Expression')]
  deriving (Show)

-- getArgumentValues :: Bindings -> Int -> Argument -> Maybe ValueFromSolution
-- getArgumentValues env nsols arg | trace ("getArgumentValues: " ++ show arg) False = undefined
-- getArgumentValues env nsols arg =
--     case arg of
--       OrdinaryParameter e ->
--           Just $ SingleValue (expressionToValue env e)
--       ErstwhileVariable _vid ->
--           Just $ ManyValues [ expressionToValue env (argumentToExpression i arg ^. expRawExpression) | i <- [1..nsols] ]
--       _a ->
--           Nothing

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

-- solve :: (Monoid a) => Bool -> Model a -> Int -> Integer -> StatisticsIO (SolveResult, [String])
-- solve restart m nsols solvingTimeout = do
-- -- liftIO $ SimpleLog.log =<< prettyPrintModel m
--  gecodePath <- liftIO $ fromMaybe "/usr/local/share/gecode/mznlib" <$> lookupEnv "GECODE_MZN"
--  statisticsTime "solving via minizinc" $ liftIO $ do
--   withTempFile "." "temp.mzn" $ \mznpath mznhandle -> do
--     hPutStrLn mznhandle (plainShow m)
--     hClose mznhandle
--     -- SimpleLog.log $ plainShow m
--     withTempFile "." "temp.fzn" $ \fznpath fznhandle -> do
--       hClose fznhandle
--       withTempFile "." "output" $ \outpath outhandle -> do
--         withTempFile "." "mzn2fzn-output" $ \stderrPath stderrHandle -> do
--           -- hNull <- openFile "/dev/null" WriteMode
--           let args = ["--no-optimise", "-I", gecodePath, "--no-output-ozn", "-o", fznpath, mznpath]
-- --          let args = ["--no-optimise", "-I", "gecode/mznlib", "--no-output-ozn", "-o", fznpath, mznpath]
--           exitCode1 <- runProcess "mzn2fzn" args Nothing Nothing Nothing Nothing (Just stderrHandle) >>= waitForProcess
--           hClose stderrHandle
--           case exitCode1 of
--             ExitFailure code -> do
--                 hPutStrLn stderr ("warning: mzn2fzn failed (exit code " ++ show code ++ ")")
--                 hPutStrLn stderr ("on this model:")
--                 hPutStrLn stderr $ plainShow m
--                 hPutStrLn stderr ("and produced this output:")
--                 hPutStrLn stderr =<< readFile stderrPath
--             ExitSuccess -> return ()
--           -- SimpleLog.log $ "mzn2fzn exit code: " ++ show exitCode1
--           randomSeed <- (randomIO :: IO Word32)
--           let args2 = [ "-n", (show nsols) ]
--                       ++ (if restart && nsols > 0 then ["-restart", "luby"] else [])
--                       ++ ["-r", show randomSeed, fznpath ]
--           hNull2 <- id $ openFile "/dev/null" WriteMode
--           let rp = runProcess "fzn-gecode" args2 Nothing Nothing Nothing (Just outhandle) (Just hNull2)
--           (ph2,exitCode2) <- flip const ("gecode" :: String) $ do
-- --            let solvingTimeout = 1
--             p <- liftIO $ rp
--             e <- liftIO $ timeout (fromIntegral (solvingTimeout*1000)) (waitForProcess p)
--             return (p, e)
--           case exitCode2 of
--             Just _ -> return ()
--             Nothing -> liftIO $ do hPutStrLn stderr "timed out"
--                                    terminateProcess ph2
--                                    hPutStrLn stderr "waiting for solver process to terminate"
--                                    void $ waitForProcess ph2
--                                    hPutStrLn stderr "terminated"
--           output <- do let attemptToRead = do
--                              r <- try (readFile outpath)
--                              case r of
--                                Right x -> return x
--                                Left exc -> do hPutStrLn stderr $ "readFile " ++ outpath ++ " raised: " ++ show (exc :: IOException)
--                                               attemptToRead
--                        attemptToRead
--           void $ evaluate $ length output
--           return $ splitSolutions output

solve :: String -> Bool -> Model -> Int -> Integer -> SimpleLog.Handle -> StatisticsIO (SolveResult, [Model])
solve dataFilePath restart m nsols solvingTimeout logHandle = timeAction "solve" $ do
-- liftIO $ SimpleLog.log =<< prettyPrintModel m
 statisticsFlatZincCall
 --gecodePath <- liftIO $ fromMaybe "/usr/local/share/gecode/mznlib" <$> lookupEnv "GECODE_MZN"
 --dataFilePath <- liftIO $ getDataFileName "data-files"
 --dataFilePath <- liftIO $ fromMaybe "data-files" <$> lookupEnv "GLOBALIZER_DIR"
 statisticsTime "solving via minizinc" $ liftIO $ do
  withTempFile "." "temp.mzn" $ \mznpath mznhandle -> do
    timeAction "solve/plainShow" $ hPutStrLn mznhandle (plainShow m)
    hClose mznhandle
    -- SimpleLog.log $ plainShow m
    withTempFile "." "temp.fzn" $ \fznpath fznhandle -> do
      hClose fznhandle
      withTempFile "." "output" $ \outpath outhandle -> do
        withTempFile "." "mzn2fzn-output" $ \stderrPath stderrHandle -> do
          -- hNull <- openFile "/dev/null" WriteMode
          let args = [ "--no-optimise"
                     , "-G", "gecode"--gecodePath
                     , "-I", dataFilePath
                     , "--no-output-ozn"
                     , "-o", fznpath
                     , mznpath]
--          let args = ["--no-optimise", "-I", "gecode/mznlib", "--no-output-ozn", "-o", fznpath, mznpath]
          SimpleLog.logN logHandle LogHigh $ "C"
          exitCode1 <-
            timeAction "solve/mzn2fzn" $
            runProcess "mzn2fzn" args Nothing Nothing Nothing Nothing (Just stderrHandle) >>= waitForProcessMsg "waiting for mzn2fzn"
          SimpleLog.logN logHandle LogHigh $ "\b"
          case exitCode1 of
            ExitFailure code -> do
                hPutStrLn stderr ("warning: mzn2fzn failed (exit code " ++ show code ++ ")")
                -- hPutStrLn stderr ("on this model:")
                -- hPutStrLn stderr $ plainShow m
                hPutStrLn stderr ("and produced this output:")
                hPutStrLn stderr =<< readFile stderrPath
                return ()
            ExitSuccess -> return ()
          -- SimpleLog.log $ "mzn2fzn exit code: " ++ show exitCode1
          randomSeed <- (randomIO :: IO Word32)
          let args2 =
                  [ "-n", (show nsols) ]
                  ++ (if restart && nsols > 0 then ["-restart", "luby"] else [])
                  ++ ["-r", show randomSeed, fznpath ]
          hNull2 <- id $ openFile "/dev/null" WriteMode
          let rp = runProcess "fzn-gecode" args2 Nothing Nothing Nothing (Just outhandle) (Just hNull2)
          (ph2,exitCode2) <- timeAction "solve/fzn-gecode" $ flip const ("gecode" :: String) $ do
            SimpleLog.log logHandle LogDebug "starting fzn-gecode"
            SimpleLog.logN logHandle LogHigh $ "G"
            p <- liftIO $ rp
            e <- liftIO $ timeout (fromIntegral (solvingTimeout*1000)) (waitForProcessMsg "waiting for fzn-gecode" p)
            SimpleLog.logN logHandle LogHigh $ "\b"
            SimpleLog.log logHandle LogDebug "fzn-gecode done/killed"
            return (p, e)
          wasKilled <- newIORef False
          case exitCode2 of
            Just _ -> return ()
            Nothing -> liftIO $ do -- hPutStrLn stderr "timed out"
                                   terminateProcess ph2
                                   -- hPutStrLn stderr "waiting for solver process to terminate"
                                   void $ waitForProcessMsg "waiting for terminated process" ph2
                                   -- hPutStrLn stderr "terminated"
                                   writeIORef wasKilled True
          output <- do let attemptToRead = do
                             r <- try (BSC.readFile outpath)
                             case r of
                               Right x -> return x
                               Left exc -> do hPutStrLn stderr $ "readFile " ++ outpath ++ " raised: " ++ show (exc :: IOException)
                                              attemptToRead
                       attemptToRead
--          void $ evaluate $ length output
          SimpleLog.log logHandle LogSolving "solution output:"
          SimpleLog.log logHandle LogSolving $ BSC.unpack output
          -- let sols = splitSolutions output
              -- l = last sols
              -- result = if l == "==========\n"
              --          then SolveComplete
              --          else SolveIncomplete
          SimpleLog.log logHandle LogSolving "parsing solver output..."
          timeAction "solve/parse" $ case Atto.parseOnly (parseSolverOutput <* Atto.endOfInput) output of
            Right (status, models) -> do
              SimpleLog.log logHandle LogSolving "parsing done"
              return (status, models)
            Left e -> do
              wk <- readIORef wasKilled
              if wk then return (SolveIncomplete, [])
                    else error $ "error parsing solutions from solver:\n" ++ show e ++ "\n" ++ BSC.unpack output


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


-- solutionDivider :: String
-- solutionDivider = replicate 10 '-'

-- splitSolutions :: String -> [String]
-- splitSolutions = map unlines . endBy [solutionDivider] . filter (not . null) . lines

-- separateAt :: Eq a => a -> [a] -> [[a]]
-- separateAt sep xs = f [] xs
--   where f acc []     = [acc]
--         f acc (y:ys) =
--             if y == sep
--             then acc : f [] ys
--             else f (acc ++ [y]) ys

type GroupName = (Integer, (DisjointLocation, Maybe (Expression)))

runGroupsModels :: String -> Bindings -> M.Map GroupName (S.Set (Model), Maybe (Expression)) -> Int -> Int -> Integer -> Maybe String -> Maybe Int -> Bool -> Int -> Bool -> ChannelMap -> SimpleLog.Handle -> StatisticsIO [[(Replacement, Double)]]
runGroupsModels dataFilePath env groupMap nRandomSolutions nSampleSolutions solvingTimeout constraintFilter selectGroup filterArgs maxArgs doImpliesCheck channelMap logHandle = do
  let ngroups = (fromIntegral $ M.size groupMap) :: Double
  let runGroup :: Integer -> (GroupName, (S.Set (Model), Maybe (Expression)))
               -> IO ([(Replacement, Double)], Statistics)
      runGroup groupNumber (ranges,(files, context)) = do
        -- For progress reporting, print the percentage done so far.
        -- This is wrong if the groups are processed in parallel.
        -- lock $ do hPrintf stderr "%.2f\n" (100.0 * fromIntegral groupNumber / ngroups)
        --           hFlush stderr
        SimpleLog.log logHandle LogHigh $ "PROCESSING GROUP " ++ show groupNumber ++ " (" ++ showDisjointLocation "" (fst (snd ranges)) ++ " [ " ++ maybe "" (showExpLocation "") (snd (snd ranges)) ++ " ])"
        SimpleLog.log logHandle LogHigh $ "  this group has " ++ show (S.size files) ++ " models"

        SimpleLog.log logHandle LogModel $ "first model in group:"
        SimpleLog.log logHandle LogModel $ plainShow (head (S.toList files))

        -- Process the group and get the output and statistics.
        (out,st) <- flip runStateT emptyStatistics $ do
                      flip catch (\AbortException -> return ([])) $ do
                        statisticsTime (T.pack ("group " ++ show groupNumber)) $ do
                          processGroupModels dataFilePath env context (S.toList files) nRandomSolutions nSampleSolutions solvingTimeout constraintFilter filterArgs maxArgs doImpliesCheck channelMap logHandle

        -- Look for channel constraints.
        -- forM_ out $ \((c, args),_) -> do
        --   when (name c == "channel") $ do
        --     let [i,bs] = args
        --     -- Get the identifiers of the arguments.
        --     let getName (ArgumentArrayAccess a args) = getName a
        --         getName (ErstwhileVariable vid) = vid
        --         getName x = "?"
        --     let iname = getName i
        --     let bsname = getName bs
        --     -- Find where the underscore is in the booleans part.  If
        --     -- it's not an array access, it's equivalent to being the
        --     -- first argument.
        --     let underscoreArg = case bs of
        --                           ErstwhileVariable vid -> 1
        --                           ArgumentArrayAccess a args -> fromMaybe (error ("couldn't find the underscore index in " ++ show args))
        --                                                         $ (+1) <$> elemIndex (Ident "_") args
        --     printf "CHANNEL: %s %s %d\n" iname bsname underscoreArg

        let isTrue ((c,args),_) = name c == "true"
        let out2 = case find isTrue out of
                     Nothing -> out
                     Just x -> [x]

        -- Make sure we force the output.
        evaluate (length (show (out2, st)))

        SimpleLog.log logHandle LogHigh $ "Group output: " ++ show out2

        return (out2, st)

  forM_ (M.toList groupMap) $ \(name, pair) -> do
    SimpleLog.log logHandle LogDebug $ "new group"
    forM_ (S.toList (fst pair)) $ \m -> do
      SimpleLog.log logHandle LogDebug $ plainShow m
    maybe (return ()) (SimpleLog.log logHandle LogDebug . showExp) (snd pair)

  -- The actions that will run all the groups.
  let actions0 = map (uncurry runGroup) (zip [0..] (M.toList groupMap))
  -- If the user selected a specific group, only run that one.
  let actions = case selectGroup of
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

-- deriveNFData ''Argument
-- deriveNFData ''ArgBaseType
-- deriveNFData ''ArgType
-- deriveNFData ''Constraint


{- From: http://stackoverflow.com/a/22674732 -}
concurrentlyLimited :: Int -> [IO a] -> IO [a]
concurrentlyLimited n tasks = concurrentlyLimited' n (zip [0..] tasks) [] []

concurrentlyLimited' _ [] [] results = return . map snd $ sortBy (comparing fst) results
concurrentlyLimited' 0 todo ongoing results = do
    (task, newResult) <- waitAny ongoing
    concurrentlyLimited' 1 todo (delete task ongoing) (newResult:results)
concurrentlyLimited' n [] ongoing results = concurrentlyLimited' 0 [] ongoing results
concurrentlyLimited' n ((i, task):otherTasks) ongoing results = do
    t <- async $ (i,) <$> task
    concurrentlyLimited' (n-1) otherTasks (t:ongoing) results



argInt0 = ArgType ArgInt 0
argInt1 = ArgType ArgInt 1
argInt2 = ArgType ArgInt 2
argInt3 = ArgType ArgInt 3

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
findM f [] = return Nothing
findM f (x:xs) = do
  b <- f x
  if b then return (Just x)
    else findM f xs


waitForProcessMsg :: String -> ProcessHandle -> IO ExitCode
waitForProcessMsg msg processHandle = do
  waitForProcess processHandle `catch` handler
  where handler e = do
          hPutStrLn stderr $ "waitForProcessMsg: " ++ msg
          throwM (e :: IOException)
