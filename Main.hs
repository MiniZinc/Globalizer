{-# LANGUAGE GADTs #-}

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Set as S
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
  main2 =<< execParser (parseOptions `withInfo` "MiniZinc Globalizer")

construct3DChannelItem o introducedLocation (dim, dimLowerLetter, dimUpperLetter,  channelName, (other1, other2)) = do
  let bri3xs = nub [ x | (_,rs) <- o, ((c,args),_) <- rs
                   , name c == "binaries_represent_int" ++ dimUpperLetter
                   , let ErstwhileVariable x = head args ]
  forM bri3xs $ \x -> do
    let xDim = x ++ dimLowerLetter
    let ti = TypeInst { tiInst = Var, tiBase = BTInt
                      , tiRanges = OrdinaryRanges [
                                     TypeInst { tiInst = Par,   tiBase = BTInt,   tiRanges = OrdinaryRanges []
                                              , tiSet  = Plain, tiOpt  = OptPlain
                                              , tiDomain = Just . mkExp $ Call ("index_set_" ++ show other1 ++ "of3") [ mkExp (Ident x) ] }
                                   , TypeInst { tiInst = Par,   tiBase = BTInt,  tiRanges = OrdinaryRanges []
                                              , tiSet  = Plain, tiOpt  = OptPlain
                                              , tiDomain = Just . mkExp $ Call ("index_set_" ++ show other2 ++ "of3") [ mkExp (Ident x) ] } ]
                      , tiSet = Plain, tiOpt = OptPlain
                      , tiDomain = (Just . mkExp) (Call ("index_set_" ++ show dim ++ "of3") [ mkExp (Ident x) ]) }
    let newVarDecl = VarDecl { _varDeclTypeInst   = ti,      _varDeclIdent       = xDim
                             , _varDeclExpression = Nothing, _varDeclAnnotations = mempty
                             , _varDeclLocation   = Nothing, _varDeclId          = Nothing }
    let newConstraint = mkExp $ Call channelName [ mkExp (Ident xDim), mkExp (Ident x) ]
    let extraItems = [ VarDeclI newVarDecl
                     , ConstraintI (newConstraint & expLocation .~ introducedLocation)]
    return (extraItems, (x, xDim, [other1,other2]))

initialPass :: GOpts.GlobalizerOptions -> SimpleLog.Handle -> IO ([Item], ChannelMap, Statistics)
initialPass opts logHandle = do
  let conFilter = Just "binaries_represent_int"
  (o,s) <- processModelAndData opts conFilter [] [] logHandle

  let introducedLocation = Just (Location (Position "introduced" (-99) (-99)) (Position "introduced" (-99) (-99)))
  extraItems3X <- mapM (construct3DChannelItem o introducedLocation) [ (1, "_3a", "_3A", "channelCAB", (2,3))
                                                                     , (2, "_3b", "_3B", "channelACB", (1,3))
                                                                     , (3, "_3c", "_3C", "channelABC", (1,2)) ]
  let extraItems = foldl (++) [] (map (concat . map fst) extraItems3X)
  let channelMap = foldl (++) [] (map (map snd) extraItems3X)

  if null extraItems
    then return ([], [], s)
    else return (IncludeI "glob.mzn" Nothing : extraItems, channelMap, s)

main2 :: GOpts.GlobalizerOptions -> IO ()
main2 opts = do
  setNumCapabilities (numJobs opts)
  logHandle <- SimpleLog.newHandle (debugging opts) stderr
  (extraItems, channelMap, initialPassStats) <-
    if doInitialPass opts
    then initialPass (opts { selectGroup = Nothing }) logHandle
    else return ([], [], emptyStatistics)
  (o,s) <- -- flip catch (\AbortException -> print "abort abort" >> return undefined) $ do
    processModelAndData opts (constraintFilter opts) extraItems channelMap logHandle
  --  putStrLn (concatMap unlines (map buildOutput o))
  -- forM_ pairedo $ \(((loc,context),m),o1) -> do
  --   let name = showDisjointLocation loc
  --              ++ " \\ "
  --              ++ fromMaybe "" ((showDisjointLocation . view expDecoration) <$> context)
  --   putStrLn name
  --   -- putStrLn $ fznShow $ head $ S.toList $ fst m
  --   putStrLn (buildOutput o1)

  -- hPutStrLn stderr $ show $ stats ^. logTree
  -- hPutStrLn stderr (showStatistics stats)

  let nameReps :: [ (GroupName, Replacement, Expression) ]
      nameReps = [ (name, replacement, constraint)
                       | x <- o,
                         let name = x ^. _1 ^. _1,
                         (replacement,_s) <- x ^. _2,
                         let constraints = [ c | ConstraintI c <- (S.findMin (x ^. _1 ^. _2 ^. _1)) ^. modelItems ],
                         let constraint = head constraints ]
  let shadowed (n,r,_) = any (\(n2,r2,_) -> (n,r) /= (n2,r2) && r == r2 && n2 `subgroupOf` n) nameReps
  let vacuous (_,r,c) = name (fst r) == toplevelCall c

  let realReplacements = filter (\x -> not (vacuous x) && not (shadowed x)) nameReps
  let modelFile = head (inputFiles opts)

  if (doOutputHTML opts)
  then do
    putStrLn("%%%mzn-html-start")
    putStrLn("<h1>Found Globals:</h1><ul>")
    mapM_ (\(((n,(l,ml))),r,c) -> putStrLn ((
      if shadowed ((n,(l,ml)),r,c)
      then "*** "
      else "") ++ (
        if vacuous ((l,ml),r,c)
        then "### "
        else "") ++ "<li><a href=\"highlight://?" ++ showDisjointLocation modelFile l ++ "&" ++ maybe "" (showExpLocation modelFile) ml ++ "\">" ++ prettyPrintify r ++ "</a></li>")) realReplacements
    putStrLn("</ul>")
    putStrLn("%%%mzn-html-end")
  else mapM_ (\(((n,(l,ml))),r,c) -> putStrLn ((
    if shadowed ((n,(l,ml)),r,c)
    then "*** "
    else "") ++ (
      if vacuous ((l,ml),r,c)
      then "### "
      else "") ++ showDisjointLocation modelFile l ++ " [ " ++ maybe "" (showExpLocation modelFile) ml ++ " ] " ++ prettyPrintify r)) realReplacements

  let allStats = s Data.Monoid.<> initialPassStats
  putStrLn $ "% NUMCALLS: " ++ show (allStats ^. numberFlatZincCalls)
  putStrLn $ "% NUMEVALS: " ++ show (allStats ^. numberModelEvaluations)

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
