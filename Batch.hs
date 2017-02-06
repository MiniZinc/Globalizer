import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.Maybe
import qualified Data.Set as S

import Language.MiniZinc

import GroupOutput
import Rewrite
import Statistics
import Types

cases =
    [ ("tests/test-cars.mzn", [], 2) ]

main = do
  setNumCapabilities 3
  t1 <- time' $ do
    forM_ cases $ \(modelFilename, dataFilenames, maxConstraints) -> do
      (o,s) <- processModelAndData maxConstraints modelFilename dataFilenames
--      mapM_ (print . view _2) o
      let numFound = sum (map (length . view _2) o)
      let nameReps = [ (name, replacement, constraint)
                           | x <- o,
                             let name = x ^. _1 ^. _1,
                             (replacement,_s) <- x ^. _2,
                             let constraints = [ c | ConstraintI c <- (S.findMin (x ^. _1 ^. _2 ^. _1)) ^. modelItems ],
                             let constraint = head constraints ]
      let shadowed (n,r,c) = any (\(n2,r2,c2) -> (n,r) /= (n2,r2) && r == r2 && n2 `subgroupOf` n) nameReps
      let unshadowed = filter (not . shadowed) nameReps
      let vacuous (n,r,c) = name (fst r) == toplevelCall c
      mapM_ (\((l,ml),r,c) -> putStrLn ((if shadowed ((l,ml),r,c) then "*** " else "") ++ (if vacuous ((l,ml),r,c) then "### " else "") ++ showDisjointLocation l ++ " [ " ++ fromMaybe "" (showDisjointLocation <$> (view expDecoration <$> ml)) ++ " ] " ++ prettyPrintify r)) nameReps
      print $ numFound
      let realReplacements = filter (\x -> not (vacuous x) && not (shadowed x)) nameReps
      mapM_ (\((l,ml),r,c) -> putStrLn ((if shadowed ((l,ml),r,c) then "*** " else "") ++ (if vacuous ((l,ml),r,c) then "### " else "") ++ showDisjointLocation l ++ " [ " ++ fromMaybe "" (showDisjointLocation <$> (view expDecoration <$> ml)) ++ " ] " ++ prettyPrintify r)) realReplacements
      -- forM_ o $ \o' -> do
      --   let constraints = [ c | x <- S.toList (o' ^. _1 ^. _2 ^. _1), ConstraintI c <- x ^. modelItems ]
      --   mapM_ (putStrLn . showExp) constraints
      --   putStrLn "-----"

  -- setNumCapabilities 1
  -- threadDelay 1000000
  -- t2 <- time' $ do
  --   forM_ cases $ \(modelFilename, dataFilenames, maxConstraints) -> do
  --     (o,s) <- processModelAndData maxConstraints modelFilename dataFilenames
  --     mapM_ (print . view _2) o
  --     print "done"
  print t1
  -- print t2


subgroupOf :: GroupName -> GroupName -> Bool
subgroupOf (loc1, mctxt1) (loc2, mctxt2) =
    or [ loc1 == loc2 && mctxt1 == Nothing
       , loc1 `sublocationOf` loc2 && mctxt1 == mctxt2 ]

sublocationOf :: DisjointLocation -> DisjointLocation -> Bool
sublocationOf loc1 loc2 = S.fromList (unDisjointLocation loc1)
                          `S.isSubsetOf`
                          S.fromList (unDisjointLocation loc2)




toplevelCall :: Expression a -> String
toplevelCall e =
    case e ^. expRawExpression of
      Call f _ -> f
      Let _ e2 -> toplevelCall e2
      e' -> showExp2 e
