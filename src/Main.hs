{-# LANGUAGE GADTs #-}

import Control.Applicative
import Control.Concurrent (setNumCapabilities)
import Control.Lens
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import System.IO
import Data.Semigroup ((<>))
import qualified Data.Monoid

import Options.Applicative as O
import SimpleLog

import Language.MiniZinc

import GroupOutput
import Loc
import Rewrite
import Statistics
import Types

import GlobalizerOptions as GOpts

main :: IO ()
main = do
  main2 =<< execParser (parseOptions `withInfo` "MiniZinc Globalizer 0.1.6.0")

makeTI inst ranges dom = 
  TypeInst { tiInst = inst
           , tiBase = BTInt
           , tiRanges = ranges
           , tiSet = Plain
           , tiOpt = OptPlain
           , tiDomain = dom
           }

makeIndexSetOf3Call dim var = Just . mkExp $ Call("index_set_" ++ show dim ++ "of3") [ mkExp (Ident var) ]

construct3DChannelItem o introducedLocation (dim, dimLowerLetter, dimUpperLetter,  channelName, (other1, other2)) = do
  let bri3xs = nub [ x | (_,rs) <- o, ((c,args),_) <- rs
                   , name c == "binaries_represent_int" ++ dimUpperLetter
                   , let ErstwhileVariable x = head args ]
  forM bri3xs $ \x -> do
    let xDim = x ++ dimLowerLetter
    let is1 = makeIndexSetOf3Call other1 x
    let is2 = makeIndexSetOf3Call other2 x
    let isd = makeIndexSetOf3Call dim x
    let ti = makeTI Var (OrdinaryRanges [ makeTI Par (OrdinaryRanges []) is1, makeTI Par (OrdinaryRanges []) is2]) isd
    let newVarDecl = VarDecl { _varDeclTypeInst   = ti,      _varDeclIdent       = xDim
                             , _varDeclExpression = Nothing, _varDeclAnnotations = mempty
                             , _varDeclLocation   = Nothing, _varDeclId          = Nothing }
    let newConstraint = mkExp $ Call channelName [ mkExp (Ident xDim), mkExp (Ident x) ]
    let extraItems = [ VarDeclI newVarDecl
                     , ConstraintI (newConstraint & expLocation .~ introducedLocation)]
    return (extraItems, (x, xDim, [other1,other2]))

initialPass :: GOpts.GlobalizerOptions -> SimpleLog.Handle -> IO ([Item], ChannelMap, Statistics, [GroupName])
initialPass opts logHandle = do
  putStrLnOut("% Globalizer: Starting initial pass (find viewpoints). Skip this pass with --no-initial-pass.")
  let includeCons = Just "binaries_represent_int,true"
  let includePaths = case GOpts.includePaths opts of
                       Just paths -> [pathsToDisjointLocation paths]
                       Nothing -> []
  let excludePaths = case GOpts.excludePaths opts of
                       Just paths -> [pathsToDisjointLocation paths]
                       Nothing -> []

  (o,s) <- processModelAndData opts 
                               (acceptableConstraint includeCons Nothing)
                               (acceptableGroup includePaths excludePaths)
                               []
                               []
                               logHandle
  putStrLnOut("% Globalizer: Finished initial pass")

  let introducedLocation = Just (Location (Position "introduced" (-99) (-99)) (Position "introduced" (-99) (-99)))
  extraItems3X <- mapM (construct3DChannelItem o introducedLocation) [ (1, "_3a", "_3A", "channelCAB", (2,3))
                                                                     , (2, "_3b", "_3B", "channelACB", (1,3))
                                                                     , (3, "_3c", "_3C", "channelABC", (1,2)) ]
  let extraItems = foldl (++) [] (map (concat . map fst) extraItems3X)
  let channelMap = foldl (++) [] (map (map snd) extraItems3X)

  let trues = nub [ gn | ((gn,(_,_)),rs) <- o
                  , any (\((c,_),_) -> name c == "true") rs]

  if null extraItems
    then return ([], [], s, trues)
    else return (IncludeI "glob.mzn" Nothing : extraItems, channelMap, s, trues)

putStrLnOut :: String -> IO()
putStrLnOut t = hPutStr stdout $ t ++ "\n"

main2 :: GOpts.GlobalizerOptions -> IO ()
main2 opts = do
  -- Set number of jobs to run in parallel
  setNumCapabilities (numJobs opts)

  -- Configure logging
  logHandle <- SimpleLog.newHandle (debugging opts) stderr

  -- Perform the initial pass
  (extraItems, channelMap, initialPassStats, trues) <-
    if doInitialPass opts
    then initialPass (opts { selectGroup = Nothing }) logHandle
    else return ([], [], emptyStatistics, [])

  -- Run the full Globalizer
  putStrLnOut("% Globalizer: Start full Globalizer pass")
  let includeCons = constraintFilterIn opts
  let excludeCons = case (constraintFilterEx opts) of
                      Nothing -> Just "true"
                      Just s -> Just $ s ++ ",true"
  let includePaths = case GOpts.includePaths opts of
                       Just paths -> [pathsToDisjointLocation paths]
                       Nothing -> []
  let excludePaths = case GOpts.excludePaths opts of
                       Just paths -> [pathsToDisjointLocation paths]
                       Nothing -> []

  (o,s) <- processModelAndData opts
                               (acceptableConstraint includeCons excludeCons)
                               (acceptableGroup includePaths (excludePaths ++ getDisjointLocations trues))
                               extraItems
                               channelMap
                               logHandle
  putStrLnOut("% Globalizer: Finished full Globalizer pass")

  printOutput opts o trues
  printStats s initialPassStats

printOutput :: GOpts.GlobalizerOptions
               -> [ (( GroupName, (S.Set (Model), Maybe (Expression)) ), [(Replacement, Double)]) ]
               -> [ GroupName ]
               -> IO ()
printOutput opts o trues = do
  -- (t ^. _1 ^. _2) accesses the second element of the first element of Tuple t
  let nameReps :: [ (GroupName, Replacement, Expression) ] 
      nameReps =  [ (name, replacement, constraint)
                       | x <- o, -- (GroupName, (S.Set (Model), Maybe (Expression)))
                         let name = x ^. _1 ^. _1,  -- Groupname
                         (replacement,_) <- x ^. _2, -- (S.Set (model), _)
                         let constraints = [ c | ConstraintI c <- (S.findMin (x ^. _1 ^. _2 ^. _1)) ^. modelItems ],
                         let constraint = head constraints ]
  let shadowed (n,r,_) = any (\(n2,r2,_) -> (n,r) /= (n2,r2) && r == r2 && n2 `subgroupOf` n) nameReps
  let vacuous (_,r,c) = name (fst r) == toplevelCall c

  let realReplacements = filter (\x -> not (vacuous x) && not (shadowed x)) nameReps
  let modelFile = head $ (filter (isSuffixOf ".mzn") (GOpts.inputFiles opts))

  if (doOutputHTML opts)
  then do
    putStrLnOut("%%%mzn-html-start")
    putStrLnOut("<h1>Redundant submodels:</h1><ul>")
    if length trues > 0 then 
      mapM_ (\l -> putStrLnOut $ "<li><a href=\"highlight://?" ++ showDisjointLocation modelFile l ++ "\">redundant/true</a></li>")
            (getDisjointLocations trues)
    else
      putStrLnOut "<li>None</li>"
    putStrLnOut("</ul>")
    putStrLnOut("<h1>Found Globals:</h1><ul>")
    mapM_ (\(((n,(l,ml))),r,c) -> putStrLnOut ((
      if shadowed ((n,(l,ml)),r,c)
      then "*** "
      else "") ++ (
        if vacuous ((l,ml),r,c)
        then "### "
        else "") ++ "<li><a href=\"highlight://?" ++ showDisjointLocation modelFile l ++ "&" ++ maybe "" (showExpLocation modelFile) ml ++ "\">" ++ prettyPrintify r ++ "</a></li>")) realReplacements
    putStrLnOut("</ul>")
    putStrLnOut("%%%mzn-html-end")
  else do
    putStrLnOut "Redundant submodels:"
    if length trues > 0 then
      mapM_ (\l -> putStrLnOut $ showDisjointLocation modelFile l ++ " [ ] redundant/true" )
            (getDisjointLocations trues)
    else
      putStrLnOut "None"

    putStrLnOut "\nFound Globals:"
    mapM_ (\(((n,(l,ml))),r,c) -> putStrLnOut ((
      if shadowed ((n,(l,ml)),r,c)
      then "*** "
      else "") ++ (
        if vacuous ((l,ml),r,c)
        then "### "
        else "") ++ showDisjointLocation modelFile l ++ " [ " ++ maybe "" (showExpLocation modelFile) ml ++ " ] " ++ prettyPrintify r)) realReplacements

printStats :: Statistics -> Statistics -> IO ()
printStats s initialPassStats = do
  let allStats = s Data.Monoid.<> initialPassStats
  putStrLnOut $ "% NUMCALLS: " ++ show (allStats ^. numberFlatZincCalls)
  putStrLnOut $ "% NUMEVALS: " ++ show (allStats ^. numberModelEvaluations)

subgroupOf :: GroupName -> GroupName -> Bool
subgroupOf (_,(loc1, mctxt1)) (_,(loc2, mctxt2)) =
    or [ loc1 == loc2 && mctxt1 == Nothing
       , loc1 `sublocationOf` loc2 && mctxt1 == mctxt2 ]

sublocationOf :: DisjointLocation -> DisjointLocation -> Bool
sublocationOf loc1 loc2 = S.fromList (unDisjointLocation loc1)
                          `S.isSubsetOf`
                          S.fromList (unDisjointLocation loc2)

toplevelCall :: Expression -> String
toplevelCall e =
    case e ^. expRawExpression of
      Call f _ -> f
      Let _ e2 -> toplevelCall e2
      _ -> showExp2 e
