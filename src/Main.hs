{-# LANGUAGE GADTs #-}

import Control.Concurrent (setNumCapabilities)
import Control.Lens
import Control.Monad
import Data.List
import Data.Text (replace, pack, unpack)
import qualified Data.Set as S
import System.IO
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
  main2 =<< execParser (parseOptions `withInfo` "MiniZinc Globalizer 0.1.7.2")

makeTI :: Inst -> Ranges -> Maybe Expression -> TypeInst
makeTI inst ranges dom = 
  TypeInst { tiInst = inst
           , tiBase = BTInt
           , tiRanges = ranges
           , tiSet = Plain
           , tiOpt = OptPlain
           , tiDomain = dom
           }

putStrLnOut :: String -> IO()
putStrLnOut t = hPutStr stdout $ t ++ "\n"

putStrLnEscOut :: String -> IO()
putStrLnEscOut t = hPutStr stdout $ t ++ "\\n"

putStrOut :: String -> IO()
putStrOut t = hPutStr stdout $ t


makeIndexSetOf3Call :: Show a => a -> String -> Maybe Expression
makeIndexSetOf3Call dim var = Just . mkExp $ Call("index_set_" ++ show dim ++ "of3") [ mkExp (Ident var) ]

construct3DChannelItem :: (Show a2, Show a1, Monad m) => [(a3, [((Constraint, [Argument]), b)])]
                                 -> Maybe Location -> (a2, [Char], [Char], String, (a1, a1)) -> m [([Item], (VarId, [Char], [a1]))]
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
  if (useSections opts)
  then do
    if (doOutputHTML opts)
    then do
      putStrLnOut("{\"type\": \"comment\", \"comment\": \"Starting initial Globalizer pass; Skip this pass with --no-initial-pass.\\n\"}")
    else
      putStrLnOut("{\"type\": \"comment\", \"comment\": \"% Globalizer: Starting initial pass; Skip this pass with --no-initial-pass.\\n\"}")
  else
    if (doOutputHTML opts)
    then do
      putStrLnOut("%%%mzn-html-start")
      putStrLnOut("Starting initial Globalizer pass; Skip this pass with --no-initial-pass.")
      putStrLnOut("%%%mzn-html-end")
    else
      putStrLnOut("% Globalizer: Starting initial pass; Skip this pass with --no-initial-pass.")

  let includeCons = Just "binaries_represent_int,true"
  let inPaths = case GOpts.includePaths opts of
                  Just paths -> [pathsToDisjointLocation paths]
                  Nothing -> []
  let exPaths = case GOpts.excludePaths opts of
                  Just paths -> [pathsToDisjointLocation paths]
                  Nothing -> []

  (o,s) <- processModelAndData opts 
                               (acceptableConstraint includeCons Nothing)
                               (acceptableGroup inPaths exPaths)
                               []
                               []
                               logHandle

  let introducedLocation = Just (Location (Position "introduced" (-99) (-99)) (Position "introduced" (-99) (-99)))
  extraItems3X <- mapM (construct3DChannelItem o introducedLocation) [ (1 :: Integer, "_3a", "_3A", "channelCAB", (2,3))
                                                                     , (2 :: Integer, "_3b", "_3B", "channelACB", (1,3))
                                                                     , (3 :: Integer, "_3c", "_3C", "channelABC", (1,2)) ]
  let extraItems = foldl (++) [] (map (concat . map fst) extraItems3X)
  let channelMap = foldl (++) [] (map (map snd) extraItems3X)

  let trues = nub [ gn | ((gn,(_,_)),rs) <- o
                  , any (\((c,_),_) -> name c == "true") rs]

  if (useSections opts)
  then do
    if (doOutputHTML opts)
    then do
      putStrLnOut("{\"type\": \"comment\", \"comment\": \"Initial pass complete: uncovered " ++ (show $ length extraItems) ++ " new viewpoints.\\n\"}")
    else do
      putStrLnOut("{\"type\": \"comment\", \"comment\": \"% Globalizer: Initial pass complete: uncovered " ++ (show $ length extraItems) ++ " new viewpoints.\\n\"}")
  else
    if (doOutputHTML opts)
    then do
      putStrLnOut("%%%mzn-html-start")
      putStrLnOut("Initial pass complete: uncovered " ++ (show $ length extraItems) ++ " new viewpoints.")
      putStrLnOut("%%%mzn-html-end")
    else do
      putStrLnOut("% Globalizer: Initial pass complete: uncovered " ++ (show $ length extraItems) ++ " new viewpoints.\n")

  if null extraItems
    then return ([], [], s, trues)
    else return (IncludeI "glob.mzn" Nothing : extraItems, channelMap, s, trues)

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
  if (useSections opts)
  then do
    if (doOutputHTML opts)
    then do
      putStrLnOut("{\"type\": \"comment\", \"comment\": \"Starting full Globalizer pass\\n\"}")
    else
      putStrLnOut("{\"type\": \"comment\", \"comment\": \"Globalizer: Start full Globalizer pass\\n\"}")
  else
    if (doOutputHTML opts)
    then do
      putStrLnOut("%%%mzn-html-start")
      putStrLnOut("Starting full Globalizer pass")
      putStrLnOut("%%%mzn-html-end")
    else
      putStrLnOut("% Globalizer: Start full Globalizer pass")

  let includeCons = constraintFilterIn opts
  let excludeCons = case (constraintFilterEx opts) of
                      Nothing -> Just "true"
                      Just s -> Just $ s ++ ",true"
  let inPaths = case GOpts.includePaths opts of
                  Just paths -> [pathsToDisjointLocation paths]
                  Nothing -> []
  let exPaths = case GOpts.excludePaths opts of
                  Just paths -> [pathsToDisjointLocation paths]
                  Nothing -> []

  (o,s) <- processModelAndData opts
                               (acceptableConstraint includeCons excludeCons)
                               (acceptableGroup inPaths (exPaths ++ getDisjointLocations trues))
                               extraItems
                               channelMap
                               logHandle
  if (useSections opts)
  then do
    if (doOutputHTML opts)
    then do
      putStrLnOut("{\"type\": \"comment\", \"comment\": \"Globalizer pass complete\"}")
    else
      putStrLnOut("{\"type\": \"comment\", \"comment\": \"% Globalizer: Finished full Globalizer pass\"}")
  else
    if (doOutputHTML opts)
    then do 
      putStrLnOut("%%%mzn-html-start")
      putStrLnOut("Globalizer pass complete")
      putStrLnOut("%%%mzn-html-end")
    else
      putStrLnOut("% Globalizer: Finished full Globalizer pass")

  printOutput opts o trues
  printStats s initialPassStats (doOutputHTML opts) (useSections opts)

-- Remove checking or assert
remove :: String -> String -> String
remove w = unpack . replace (pack w) (pack "") . pack

cleanName :: String -> String
cleanName = remove "_checking" . remove "_assert"

-- Map internal names to Global Constraint Catalog names
gccatName :: String -> String
gccatName = gccatName' . cleanName

gccatName' :: String -> String
gccatName' conname
  | conname == "all_different"             = "alldifferent"
  | conname == "alldifferent_except_0"     = "alldifferent_except_0"
  | conname == "all_equal_int"             = "all_equal_int"
  | conname == "atleast"                   = "atleast"
  | conname == "atmost"                    = "atmost"
  | conname == "bin_packing"               = "bin_packing"
  | conname == "bin_packing_capa"          = "bin_packing_capa"
  | conname == "bin_packing_load"          = "bin_packing"
  | conname == "bin_packing_load_ub"       = "bin_packing"
  | conname == "circuit"                   = "circuit"
  | conname == "count"                     = "count"
  | conname == "count_geq"                 = "count"
  | conname == "cumulative"                = "cumulative"
  | conname == "decreasing"                = "decreasing"
  | conname == "diffn"                     = "diffn"
  | conname == "distribute"                = "global_cardinality"
  | conname == "element"                   = "element"
  | conname == "exactly"                   = "exactly"
  | conname == "gcc"                       = "global_cardinality"
  | conname == "global_cardinality"        = "global_cardinality"
  | conname == "inverse"                   = "inverse"
  | conname == "increasing"                = "increasing"
  | conname == "lex_greater"               = "lex_greater"
  | conname == "lex_greatereq"             = "lex_greatereq"
  | conname == "lex_less"                  = "lex_less"
  | conname == "lex_lesseq"                = "lex_lesseq"
  | conname == "lex2"                      = "lex2"
  | conname == "maximum"                   = "maximum"
  | conname == "minimum"                   = "minimum"
  | conname == "member"                    = "in"
  | conname == "nvalue"                    = "nvalue"
  | conname == "sliding_sum"               = "sliding_sum"
  | conname == "sort"                      = "sort"
  | conname == "strict_lex2"               = "strict_lex2"
  | conname == "sum_constraint"            = "sum_constraint"
  | conname == "value_precede"             = "value_precede"
  | conname == "true"                      = "true"
  | conname == "subcircuit"                = "unknown"
  | otherwise                              = "unknown"

printOutput :: GOpts.GlobalizerOptions
               -> [ (( GroupName, (S.Set (Model), Maybe (Expression)) ), [(Replacement, Double)]) ]
               -> [ GroupName ]
               -> IO ()
printOutput opts o trues = do
  -- (t ^. _1 ^. _2) accesses the second element of the first element of Tuple t
  let nameReps :: [ (GroupName, Replacement, Expression) ] 
      nameReps =  [ (ident, replacement, constraint)
                       | x <- o, -- (GroupName, (S.Set (Model), Maybe (Expression)))
                         let ident = x ^. _1 ^. _1,  -- Groupname
                         (replacement,_) <- x ^. _2, -- (S.Set (model), _)
                         let constraints = [ c | ConstraintI c <- (S.findMin (x ^. _1 ^. _2 ^. _1)) ^. modelItems ],
                         let constraint = head constraints ]
  let shadowed (n,r,_) = any (\(n2,r2,_) -> (n,r) /= (n2,r2) && r == r2 && n2 `subgroupOf` n) nameReps
  let vacuous (_,r,c) = name (fst r) == toplevelCall c

  let realReplacements = filter (\x -> not (vacuous x) && not (shadowed x)) nameReps
  let modelFile = head $ (filter (isSuffixOf ".mzn") (GOpts.inputFiles opts))

  if (useSections opts)
  then do
    if (doOutputHTML opts)
    then do
      putStrOut("{\"type\": \"solution\", \"output\": { \"html\": \"")

      if length trues > 0 then do
        putStrOut("<h2>Redundant submodels:</h2>")
        mapM_ (\l -> putStrOut $ "<br>&nbsp;&#8226;&nbsp;<a href=\\\"highlight://?" ++ showDisjointLocation modelFile l ++ "\\\">redundant/true</a>")
              (getDisjointLocations trues)
        putStrOut("<br>")
      else
        return ()

      putStrOut("<h2>Found Globals:</h2>")
      mapM_ (\(((n,(l,ml))),r,c) -> putStrOut ((
        if shadowed ((n,(l,ml)),r,c)
        then "*** "
        else "") ++ (
          if vacuous ((l,ml),r,c)
          then "### "
          else "") ++ "<br>&nbsp;&#8226;&nbsp;" ++ prettyPrintify r ++
            " [<a href=\\\"highlight://?" ++ showDisjointLocation modelFile l ++ "&" ++ maybe "" (showExpLocation modelFile) ml ++ "\\\">Highlight</a>," ++
            "<a href=\\\"https://www.minizinc.org/doc-2.5.5/en/lib-globals.html?highlight=" ++ constraintName (name (fst r)) ++ "\\\">Docs</a>," ++
            -- "<a href=\\\"http://localhost:8000/lib-globals.html?highlight=" ++ cleanName (constraintName (name (fst r))) ++ "\\\">Docs</a>," ++
            "<a href=\\\"https://sofdem.github.io/gccat/gccat/C" ++ gccatName (constraintName (name (fst r))) ++ ".html\\\">GCCatalog</a>]" ++
            "</li>")) realReplacements
      putStrOut("<br>")
      putStrLnOut("\"}}")
    else do
      putStrOut "{\"type\": \"solution\", \"output\": { \"raw\": \""
      if length trues > 0 then do
        putStrLnEscOut "Redundant submodels:"
        if length trues > 0 then
          mapM_ (\l -> putStrLnEscOut $ showDisjointLocation modelFile l ++ " [ ] redundant/true" )
                (getDisjointLocations trues)
        else
          putStrLnEscOut "None"
      else
        return ()

      putStrLnEscOut "\\nFound Globals:"
      mapM_ (\(((n,(l,ml))),r,c) -> putStrLnEscOut ((
        if shadowed ((n,(l,ml)),r,c)
        then "*** "
        else "") ++ (
          if vacuous ((l,ml),r,c)
          then "### "
          else "") ++ showDisjointLocation modelFile l ++ " [ " ++ maybe "" (showExpLocation modelFile) ml ++ " ] " ++ prettyPrintify r)) realReplacements
      putStrLnOut "\"}}"
  else do
    if (doOutputHTML opts)
    then do
      putStrLnOut("%%%mzn-html-start")

      if length trues > 0 then do
        putStrLnOut("<h2>Redundant submodels:</h2>")
        mapM_ (\l -> putStrLnOut $ "<br>&nbsp;&#8226;&nbsp;<a href=\"highlight://?" ++ showDisjointLocation modelFile l ++ "\">redundant/true</a>")
              (getDisjointLocations trues)
        putStrLnOut("<br>")
      else
        return ()


      putStrLnOut("<h2>Found Globals:</h2>")
      mapM_ (\(((n,(l,ml))),r,c) -> putStrLnOut ((
        if shadowed ((n,(l,ml)),r,c)
        then "*** "
        else "") ++ (
          if vacuous ((l,ml),r,c)
          then "### "
          else "") ++ "<br>&nbsp;&#8226;&nbsp;" ++ prettyPrintify r ++
            " [<a href=\"highlight://?" ++ showDisjointLocation modelFile l ++ "&" ++ maybe "" (showExpLocation modelFile) ml ++ "\">Highlight</a>," ++
            "<a href=\"https://www.minizinc.org/doc-2.5.5/en/lib-globals.html?highlight=" ++ constraintName (name (fst r)) ++ "\">Docs</a>," ++
            -- "<a href=\"http://localhost:8000/lib-globals.html?highlight=" ++ cleanName (constraintName (name (fst r))) ++ "\">Docs</a>," ++
            "<a href=\"https://sofdem.github.io/gccat/gccat/C" ++ gccatName (constraintName (name (fst r))) ++ ".html\">GCCatalog</a>]" ++
            "</li>")) realReplacements
      putStrLnOut("<br>")
      putStrLnOut("%%%mzn-html-end")
    else do
      if length trues > 0 then do
        putStrLnOut "Redundant submodels:"
        if length trues > 0 then
          mapM_ (\l -> putStrLnOut $ showDisjointLocation modelFile l ++ " [ ] redundant/true" )
                (getDisjointLocations trues)
        else
          putStrLnOut "None"
      else
        return ()

      putStrLnOut "\nFound Globals:"
      mapM_ (\(((n,(l,ml))),r,c) -> putStrLnOut ((
        if shadowed ((n,(l,ml)),r,c)
        then "*** "
        else "") ++ (
          if vacuous ((l,ml),r,c)
          then "### "
          else "") ++ showDisjointLocation modelFile l ++ " [ " ++ maybe "" (showExpLocation modelFile) ml ++ " ] " ++ prettyPrintify r)) realReplacements

printStats :: Statistics -> Statistics -> Bool -> Bool -> IO ()
printStats s initialPassStats html useSects = do
  let allStats = s Data.Monoid.<> initialPassStats
  if useSects then do
    putStrLnOut ("{\"type\": \"statistics\", \"statistics\": { \"NUMCALLS\": " ++ show (allStats ^. numberFlatZincCalls) ++ ", \"NUMEVALS\": " ++ show (allStats ^. numberModelEvaluations) ++ "}}")
  else
    if html then do
      putStrLnOut ("%%%mzn-html-start\n" ++
       "NUMCALLS: " ++ show (allStats ^. numberFlatZincCalls) ++
       "<br>NUMEVALS: " ++ show (allStats ^. numberModelEvaluations) ++
       "\n%%%mzn-html-end\n")
    else do
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
