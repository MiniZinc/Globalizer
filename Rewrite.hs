{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Rewrite where

import Language.MiniZinc
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Applicative
import Control.Lens hiding (contexts, Context)
import Control.Monad.State
import System.Environment
import Data.Either
import Data.Data
import Data.Data.Lens
import Data.List
import qualified Data.Graph.Inductive as Gr
import Language.MiniZinc.Bindings
import Language.MiniZinc.Resolve
import qualified Data.Set as S
import qualified Data.Map as M
import Text.Printf
import System.IO

import GroupOutput (runGroupsModels, buildOutput, GroupName)
import Loc
import Misc
import Normalisation
import Statistics
import Submodel
import Types
import Transform

import SimpleLog

import Paths_minizinc_globalizer

import Prelude hiding (log)

--import Debug.Trace (trace)

-- Given a model, return its submodel groups.
--
-- There is a submodel group for each possible pair of submodel &
-- context (the context may be absent).  Within a group, the unrolled
-- variables are instantiated differently.
newSubmodel :: Int -> Model -> [([Model],Maybe (Expression))]
newSubmodel maxConstraints m = do
  let otherItems = filter (not . isConstraintI) (m ^. modelItems)
  (cs,ctexts) <- newSubmodel' m
  guard (length cs <= maxConstraints)
  let instantiations = instantiate cs
  let models = do assigns <- instantiations
                  return $ Model $ otherItems ++ assigns
  c <- Nothing : map Just ctexts
--  guard (c == Nothing)
  return (models, c)
  

-- Given a model, return its possible submodels.
--
-- A submodel consists of a set of possibly-unrolled constraints and a
-- set of possible contexts.  Each constraint is paired with the set
-- of variables that have been unrolled (each with the set of values
-- that are its domain).  Each context is simply an unaltered
-- constraint.
newSubmodel' :: Model -> [([(Expression, [(VarDecl, Expression)])], [Expression])]
newSubmodel' m = f (m ^. modelItems) [] []
  where f [] targetConstraints contexts =
          return (targetConstraints, contexts)
        f ((ConstraintI c):is) targetConstraints contexts = do
          -- Keep (and unroll) or discard?
          discard <- [True, False]
          -- If we're not using the constraint as part of the target,
          -- we can use it as a context.
          if discard then f is targetConstraints (c:contexts)
            else do
              -- Take a variant of the constraint.  This includes a
              -- non-unrolled variant.
              (cv, unrollset) <- newvariants c
              -- Add the variant to the constraint set.
              let newConstraints = ((cv,unrollset):targetConstraints)
              -- Check that all constraints still have the same set of
              -- unrolled variables.
              let condition = allEq (map (\(_cv,ps) -> sort (map stripMetadataExp (map snd ps))) newConstraints)
              guard $ condition
              -- Continue.
              f is newConstraints contexts
        f (_i:is) targetConstraints contexts =
          f is targetConstraints contexts


instantiate :: [(Expression, [(VarDecl, Expression)])] -> [[Item]]
instantiate [] = return []
instantiate ((constraint, []):newConstraints) =
  return $ map ConstraintI (constraint : map fst newConstraints)
instantiate ((constraint, pairs):newConstraints) = do
  choice <- do -- The "value-combo" is a combination of choices for
               -- each variable.  E.g.
               --   forall (i in S, j in S, k in T)
               -- might yield
               --   [ ("int:LEADER_i=min(S)", "S", "min(S)")
               --   , ("int:LEADER_j=max(S)", "S", "max(S)")
               --   , ("int:LEADER_k=min(T)", "T", "min(T)") ]
               valueCombo <- forM pairs $ \(vd,valueset) ->
                 -- For a given unrolled variable, e.g. "i in S"
                 do -- Choose a value ("min(S)" or "max(S)")
                    value <- [ locExp $ Call "min" [valueset]
                             , locExp $ Call "max" [valueset] ]
                    -- Create a new vardecl, e.g. "int:LEADER_i = min(S)"
                    let newIdent = "LEADER_" ++ (vd ^. varDeclIdent)
                        newVD = vd & varDeclExpression .~ Just value
                                   & varDeclIdent .~ newIdent
                    return (newVD, valueset, value)
               let newVarDecls = map (view _1) valueCombo
                   newTriples = map (\(vd,valueset,value) -> (vd ^. varDeclIdent, valueset, value)) valueCombo
               return (newVarDecls, newTriples)
  
  let (newVDs, triples) = choice
  rest <- f triples ((constraint, pairs):newConstraints)
  return $ map VarDeclI newVDs ++ map ConstraintI rest
  where f _pairs' [] = return []
        f pairs' ((c2,pairs2):cs) = do
          -- Our task is to connect the variables in this constraint
          -- with the values from the chosen value-combo.

          -- The current variable "v2" is compatible with a value from
          -- the value-combo if they have the same value set.
          let compatible (_leaderident,vs1,_value) (_v2,vs2) = vs1 == (stripMetadataExp vs2)
          -- Match the variables up with values from the value-combo.
          assignPairs <- pairUp compatible pairs' pairs2
          -- Make variable declarations that assign those values.
          let newVDs = map (\((leaderident,_leaderset,_leadervalue),(vd,_vidset)) ->
                               vd & varDeclExpression .~ Just (locExp $ Ident leaderident)) assignPairs
          -- Stick a "let" expression on the front of the constraint.
          let c = locExp $ Let (map VarDeclI newVDs) c2
          rest <- f pairs' cs
          return $ c : rest

processModelAndData :: Int -> FilePath -> [FilePath] -> Int -> Int -> Integer -> Maybe String -> Maybe Int -> Bool -> Int -> Bool -> [Item] -> ChannelMap -> SimpleLog.Handle
                    -> IO (
                           [
                            (( GroupName, (S.Set (Model), Maybe (Expression)) ),
                             [(Replacement, Double)])
                           ],
                           Statistics
                          )
processModelAndData maxConstraints s datafiles nRandomSolutions nSampleSolutions solvingTimeout constraintFilter selectGroup filterArgs maxArgs doImpliesCheck extraItems channelMap logHandle = do
  ((o,modelMap),stats) <- runStatistics $ do
    originalModel0 <- liftIO (readModel s)
    let originalModel = originalModel0 & modelItems %~ (++extraItems)
    let normalisedModel = rewriteModel initialNormalisation originalModel
    log logHandle LogNormalisation $ unlines
      [ ""
      , "NORMALISATION"
      , ""
      , "before:"
      , plainShow originalModel
      , ""
      , "after:"
      , plainShow normalisedModel
      , "" ]
    let gs = getGroups maxConstraints normalisedModel

    -- liftIO $ forM_ gs $ \ (ms, context) -> do
    --   logN logHandle LogHigh "\nGROUP, "
    --   let loc = modelConstraintLocations (head ms)
    --   log logHandle LogHigh $ "location: " ++ showDisjointLocation loc
    --   log logHandle LogHigh $ "context: " ++ case context of
    --                                            Just e -> showExp e
    --                                            Nothing -> "<none>"
    --   log logHandle LogHigh $ ""
    --   forM_ ms $ \m -> do
    --     log logHandle LogHigh $ plainShow $ m

    -- statisticsTime "groups" $ do
    --   forM_ (zip [0..] gs) $ \(n,g) -> do
    --     statisticsTime (T.pack (show n)) $ do
    --       recordLogKey "model" (plainShow (head (fst g)))

    liftIO $ putStrLn $ "NUMGROUPS: " ++ show (length gs)

    ds' <- liftIO (mapM readModel datafiles)
    let ds = case ds' of
               [] -> [Model []]
               _ -> ds'

    bothglobMznPath <- liftIO $ getDataFileName "data-files/both-glob.mzn"
    globalsModel <- liftIO (stripMetadataModel . (either (const (error "no both-glob.mzn?")) id) <$> parseModelFile bothglobMznPath)
    let env = topLevelBindings (stripMetadataModel globalsModel)

    modelSets <- forM (zip [0..] gs) $ \(n,(g,context)) -> statisticsTime (T.pack ("model set " ++ show n)) $ do
      let name = (n,(modelConstraintLocations (head g), context))
          ms = [ rewriteModel afterDataNormalisation (m & modelItems <>~ d ^. modelItems) | m <- map stripMetadataModel g, d <- map stripMetadataModel ds ]
      recordLogKey "name" (show name)
      recordLogKey "after data norm" (plainShow (head ms))
      let ms' = filter (connected . rewriteOf ignored removeTrivialConstraints) ms
      recordLogKey "num connected submodels" (show (length ms'))
      return $ id
             $ (name, (S.fromList ms', (stripMetadataExp <$> context)))
    let modelSets' = filter (not . S.null . fst . snd) (modelSets)
    let modelMap = M.fromList modelSets'
    o <- runGroupsModels env modelMap nRandomSolutions nSampleSolutions solvingTimeout constraintFilter selectGroup filterArgs maxArgs doImpliesCheck channelMap logHandle
    return (o, modelMap)
  return (zip (M.toList modelMap) o,stats)

readModel :: FilePath -> IO (Model)
readModel f = (either (error.show) id . parseString model)  <$> readFile f

-- Is the constraint graph of this model connected?
connected :: Model -> Bool
connected m =
    not (null (filter isConstraintI (m ^. modelItems))) &&
    let constraints = filter isConstraintI (m ^. modelItems)
        nodes = zip [1..] constraints
        edges = [ (n1,n2,()) | (n1,c1) <- nodes, (n2,c2) <- nodes, hasEdge c1 c2 ]
        hasEdge (ConstraintI c1) (ConstraintI c2) = not (null (intersect (unboundIdentifiersAfterResolution c1) (unboundIdentifiersAfterResolution c2)))
        connectionGraph = mkGr nodes edges
        mkGr :: [Gr.LNode n] -> [Gr.LEdge e] -> Gr.Gr n e
        mkGr = Gr.mkGraph
        result = Gr.isConnected connectionGraph
    in result

testGroups filename = do
  originalModel <- liftIO (readModel filename)
  let maxConstraints = 1
  let normalisedModel = rewriteModel initialNormalisation originalModel
  -- log logHandle LogDebug (plainShow originalModel)
  -- log logHandle LogDebug (plainShow normalisedModel)
  return $ getGroups maxConstraints normalisedModel

testMain = testMain' "tests/test-unroll.mzn"
testMain' filename = do
  gs <- testGroups filename
  forM_ gs $ \ (ms, context) -> do
    putStrLn "\ngroup:\n"
    let loc = modelConstraintLocations (head ms)
    putStrLn $ "location: " ++ showDisjointLocation "unknown" loc
    forM_ ms $ \m -> do
      putStrLn . plainShow $ m

-- main :: IO ()
-- main = do
--   (m:d) <- getArgs
--   test' m d

