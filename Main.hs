{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
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

data Options = Options { inputFiles :: [String]
                       , dataFiles :: [String]
                       , maxConstraints :: Int
                       , nRandomSolutions :: Int
                       , nSampleSolutions :: Int
                       , debugging :: [LogCategory]
                       , includeDir :: Maybe String
                       , constraintFilter :: Maybe String
                       , solvingTimeout :: Integer
                       , selectGroup :: Maybe Int
                       , numJobs :: Int
                       , filterArguments :: Bool
                       , maxArguments :: Int
                       , doInitialPass :: Bool
                       , doImpliesCheck :: Bool
                       , doOutputHTML :: Bool
                       }
  deriving (Show)

main :: IO ()
main = do
  main2 =<< execParser (parseOptions `withInfo` "MiniZinc Globalizer")

withInfo :: O.Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseOptions :: O.Parser Options
parseOptions = Options
                 <$> parseArguments
                 <*> parseDataFiles
                 <*> parseMaxConstraints
                 <*> parseRandomSolutions
                 <*> parseSampleSolutions
                 <*> parseDebugging
                 <*> parseIncludeDir
                 <*> parseConstraintFilter
                 <*> parseSolvingTimeout
                 <*> parseSelectGroup
                 <*> parseNumJobs
                 <*> parseFilterArguments
                 <*> parseMaxArguments
                 <*> parseDoInitialPass
                 <*> parseDoImpliesCheck
                 <*> parseDoOutputHTML

parseSelectGroup :: O.Parser (Maybe Int)
parseSelectGroup = optional $ option auto $
  long "selectGroup"
  <> metavar "<index>"
  <> help "Consider only the selected group"


parseMaxConstraints :: O.Parser Int
parseMaxConstraints = option auto $
  short 'm' <> long "maxConstraints"
  <> metavar "<num>"
  <> help "Max constraints per set"
  <> value 2
  <> showDefault

parseMaxArguments :: O.Parser Int
parseMaxArguments = option auto $
  long "maxArguments"
  <> metavar "<num>"
  <> help "Max argument lists for a constraint"
  <> value 50000
  <> showDefault

parseRandomSolutions :: O.Parser Int
parseRandomSolutions = option auto $
  short 'r' <> long "randomSolutions"
  <> metavar "<num>"
  <> help "Number of solutions found for submodel"
  <> value 30
  <> showDefault

parseSampleSolutions :: O.Parser Int
parseSampleSolutions = option auto $
  short 's' <> long "sampleSolutions"
  <> metavar "<num>"
  <> help "Number of solutions found for constraint scoring"
  <> value 30
  <> showDefault

parseArguments :: O.Parser [String]
parseArguments = some $ O.argument str $
  help "Input files (.mzn and .dzn, with the .mzn first)"
  <> metavar "<input files>"

parseDataFiles :: O.Parser [String]
parseDataFiles = some $ strOption $
  short 'd'
  <> help "Data files (.dzn)"
  <> metavar "<data files>"

parseDebugging :: O.Parser [LogCategory]
parseDebugging =
  let list :: [O.Parser (Maybe LogCategory)]
      list = do (category, opt, desc) <- cats
                return $ O.flag Nothing (Just category) $
                  (long ("debug-"++opt) <> help ("Produce debugging output related to "++desc))
  in catMaybes <$> sequenceA list
  where
    cats = [ (LogArgs, "args", "argument generation")
           , (LogConstraints, "constraints", "constraint checking")
           , (LogScoring, "scoring", "constraint scoring")
           , (LogChecking, "checking", "constraint checking")
           , (LogNormalisation, "norm", "model normalisation")
           , (LogSolving, "solving", "model solving")
           , (LogModel, "model", "group models")
           , (LogHigh, "high", "high-level progress") ]

-- parseDebugging = O.switch $
--   long "debug"
--   <> help "Produce debugging output"

-- parseDebuggingArgs :: O.Parser Bool
-- parseDebuggingArgs = O.switch $
--   long "debug-args"
--   <> help "Produce debugging output related to argument generation"

parseConstraintFilter :: O.Parser (Maybe String)
parseConstraintFilter = optional $ option str $
  short 'f' <> long "constraintFilter"
  <> metavar "<substring>"
  <> help "Consider only constraints containing this substring"

parseIncludeDir :: O.Parser (Maybe String)
parseIncludeDir = optional $ option str $
  short 'I' <> long "search-dir"
  <> metavar "<dir>"
  <> help "Additionally search for included files in <dir>"

parseSolvingTimeout :: O.Parser Integer
parseSolvingTimeout = option auto $
  short 't' <> long "solvingTimeout"
  <> metavar "<time>"
  <> help "Timeout for solving (in milliseconds)"
  <> value 1000
  <> showDefault

parseNumJobs :: O.Parser Int
parseNumJobs = option auto $
  short 'j' <> long "jobs"
  <> metavar "<num>"
  <> help "Number of jobs to run in parallel"
  <> value 1
  <> showDefault

parseFilterArguments :: O.Parser Bool
parseFilterArguments = option auto $
  long "filterArguments"
  <> metavar "<bool>"
  <> help "Whether to filter potential arguments for constraints"
  <> value True
  <> showDefault

parseDoInitialPass :: O.Parser Bool
parseDoInitialPass = flag True False $
  long "no-initial-pass"
  <> help "Don't run the initial pass to find channeling opportunities"
  <> showDefault

parseDoImpliesCheck :: O.Parser Bool
parseDoImpliesCheck = flag True False $
  long "no-implies-check"
  <> help "Don't run the implies check"
  <> showDefault

parseDoOutputHTML :: O.Parser Bool
parseDoOutputHTML = flag False True $
  long "output-html"
  <> help "Output HTML for MiniZincIDE"
  <> showDefault




initialPass :: Options -> SimpleLog.Handle -> IO ([Item], ChannelMap, Statistics)
initialPass opts logHandle = do
  let conFilter = Just "binaries_represent_int"
  (o,s) <-
    processModelAndData (includeDir opts) (maxConstraints opts) (head (inputFiles opts)) ((tail (inputFiles opts)) ++ (dataFiles opts)) (nRandomSolutions opts) (nSampleSolutions opts) (solvingTimeout opts) conFilter (selectGroup opts) (filterArguments opts) (maxArguments opts) (doImpliesCheck opts) [] [] logHandle

  let introducedLocation = Just (Location (Position "introduced" (-99) (-99))
                                          (Position "introduced" (-99) (-99)))
  -- forM_ o $ \(g,rs) -> do
  --   print rs
  -- print "initial pass done"

  -- Is there a binaries_represent_int_3C ?
  let bri3cs = nub [ x | (_,rs) <- o, ((c,args),_) <- rs, name c == "binaries_represent_int_3C",
                         let ErstwhileVariable x = head args ]
  extraItems3c <- Control.Monad.forM bri3cs $ \x -> do
    let ti = TypeInst { tiInst = Var
                      , tiBase = BTInt
                      , tiRanges = OrdinaryRanges [
                                    TypeInst { tiInst = Par, tiBase = BTInt, tiRanges = OrdinaryRanges [], tiSet = Plain, tiOpt = OptPlain, tiDomain = (Just . mkExp) (Call "index_set_1of3" [ mkExp (Ident x) ]) }
                                  , TypeInst { tiInst = Par, tiBase = BTInt, tiRanges = OrdinaryRanges [], tiSet = Plain, tiOpt = OptPlain, tiDomain = (Just . mkExp) (Call "index_set_2of3" [ mkExp (Ident x) ]) }
                                   ]
                      , tiSet = Plain
                      , tiOpt = OptPlain
                      , tiDomain = (Just . mkExp) (Call "index_set_3of3" [ mkExp (Ident x) ]) }
    let newVarDecl = VarDecl { _varDeclTypeInst = ti
                             , _varDeclIdent = x ++ "_3c"
                             , _varDeclExpression = Nothing
                             , _varDeclAnnotations = mempty
                             , _varDeclLocation = Nothing
                             , _varDeclId = Nothing }
    let newConstraint = mkExp $ Call "channelABC" [ mkExp (Ident (x ++ "_3c")), mkExp (Ident x) ]
    let extraItems = [ VarDeclI newVarDecl
                     , ConstraintI (newConstraint & expLocation .~ introducedLocation)
                     ]
    return (extraItems, (x, x ++ "_3c", [1,2]))

  -- Is there a binaries_represent_int_3B ?
  let bri3bs = nub [ x | (_,rs) <- o, ((c,args),_) <- rs, name c == "binaries_represent_int_3B",
                         let ErstwhileVariable x = head args ]
  extraItems3b <- Control.Monad.forM bri3bs $ \x -> do
    let ti = TypeInst { tiInst = Var
                      , tiBase = BTInt
                      , tiRanges = OrdinaryRanges [
                                    TypeInst { tiInst = Par, tiBase = BTInt, tiRanges = OrdinaryRanges [], tiSet = Plain, tiOpt = OptPlain, tiDomain = (Just . mkExp) (Call "index_set_1of3" [ mkExp (Ident x) ]) }
                                  , TypeInst { tiInst = Par, tiBase = BTInt, tiRanges = OrdinaryRanges [], tiSet = Plain, tiOpt = OptPlain, tiDomain = (Just . mkExp) (Call "index_set_3of3" [ mkExp (Ident x) ]) }
                                   ]
                      , tiSet = Plain
                      , tiOpt = OptPlain
                      , tiDomain = (Just . mkExp) (Call "index_set_2of3" [ mkExp (Ident x) ]) }
    let newVarDecl = VarDecl { _varDeclTypeInst = ti
                             , _varDeclIdent = x ++ "_3b"
                             , _varDeclExpression = Nothing
                             , _varDeclAnnotations = mempty
                             , _varDeclLocation = Nothing
                             , _varDeclId = Nothing }
    let newConstraint = mkExp $ Call "channelACB" [ mkExp (Ident (x ++ "_3b")), mkExp (Ident x) ]
    let extraItems = [ VarDeclI newVarDecl
                     , ConstraintI (newConstraint & expLocation .~ introducedLocation)
                     ]
    return (extraItems, (x, x ++ "_3b", [1,3]))

  -- Is there a binaries_represent_int_3A ?
  let bri3as = nub [ x | (_,rs) <- o, ((c,args),_) <- rs, name c == "binaries_represent_int_3A",
                         let ErstwhileVariable x = head args ]
  extraItems3a <- Control.Monad.forM bri3as $ \x -> do
    let ti = TypeInst { tiInst = Var
                      , tiBase = BTInt
                      , tiRanges = OrdinaryRanges [
                                    TypeInst { tiInst = Par, tiBase = BTInt, tiRanges = OrdinaryRanges [], tiSet = Plain, tiOpt = OptPlain, tiDomain = (Just . mkExp) (Call "index_set_2of3" [ mkExp (Ident x) ]) }
                                  , TypeInst { tiInst = Par, tiBase = BTInt, tiRanges = OrdinaryRanges [], tiSet = Plain, tiOpt = OptPlain, tiDomain = (Just . mkExp) (Call "index_set_3of3" [ mkExp (Ident x) ]) }
                                   ]
                      , tiSet = Plain
                      , tiOpt = OptPlain
                      , tiDomain = (Just . mkExp) (Call "index_set_1of3" [ mkExp (Ident x) ]) }
    let newVarDecl = VarDecl { _varDeclTypeInst = ti
                             , _varDeclIdent = x ++ "_3a"
                             , _varDeclExpression = Nothing
                             , _varDeclAnnotations = mempty
                             , _varDeclLocation = Nothing
                             , _varDeclId = Nothing }
    let newConstraint = mkExp $ Call "channelCAB" [ mkExp (Ident (x ++ "_3a")), mkExp (Ident x) ]
    let extraItems = [ VarDeclI newVarDecl
                     , ConstraintI (newConstraint & expLocation .~ introducedLocation)
                     ]
    return (extraItems, (x, x ++ "_3a", [2,3]))

  let extraItems = concat (map fst extraItems3a) ++ concat (map fst extraItems3b) ++ concat (map fst extraItems3c)
  let channelMap = (map snd extraItems3a) ++ (map snd extraItems3b) ++ (map snd extraItems3c)
--  mapM_ (putStrLn . showItem) extraItems
  if null extraItems
    then return ([], [], s)
    else return (IncludeI "glob.mzn" Nothing : extraItems, channelMap, s)

main2 :: Options -> IO ()
main2 opts = do
  setNumCapabilities (numJobs opts)
  logHandle <- SimpleLog.newHandle (debugging opts) stderr
  (extraItems, channelMap, initialPassStats) <-
    if doInitialPass opts
    then initialPass (opts { selectGroup = Nothing }) logHandle
    else return ([], [], emptyStatistics)
  (o,s) <- -- flip catch (\AbortException -> print "abort abort" >> return undefined) $ do
    processModelAndData (includeDir opts) (maxConstraints opts) (head (inputFiles opts)) ((tail (inputFiles opts)) ++ (dataFiles opts)) (nRandomSolutions opts) (nSampleSolutions opts) (solvingTimeout opts) (constraintFilter opts) (selectGroup opts) (filterArguments opts) (maxArguments opts) (doImpliesCheck opts) extraItems channelMap logHandle
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
  
  -- print $ numFound
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
  putStrLn $ "NUMCALLS: " ++ show (allStats ^. numberFlatZincCalls)
  putStrLn $ "NUMEVALS: " ++ show (allStats ^. numberModelEvaluations)

  -- forM_ o $ \o' -> do
  --   let constraints = [ c | x <- S.toList (o' ^. _1 ^. _2 ^. _1), ConstraintI c <- x ^. modelItems ]
  --   mapM_ (putStrLn . showExp) constraints
  --   putStrLn "-----"
--  B.writeFile "log.json" $ encodePretty $ nestedLogToJSON $ logTreeToNestedLog $ s ^. logTree
--  print $ logTreeToNestedLog $ s ^. logTree


-- parseArgs :: [String] -> Options
-- parseArgs args = case runParser parser (defaultOptions, NoModelYet) "" args of
--                    Left e -> error (show e)
--                    Right x -> x

-- data ParseState = NoModelYet
--                 | GotModel

-- parser :: ParsecT [String] (Options, ParseState) Identity Options
-- parser =
--   do a <- arg
--      case a of
--        "-m" -> do
--               x <- arg
--               updateState (_1 . maxConstraints .~ read x)
--               parser
--        f | Just x <- stripPrefix "-m" f -> do
--                        updateState (_1 . maxConstraints .~ read x)
--                        parser
--        f -> do s <- getState
--                case snd s of
--                  NoModelYet -> do updateState (_1 . modelFile .~ f)
--                                   updateState (_2 .~ GotModel)
--                  GotModel -> updateState (_1 . dataFiles <>~ [f])
--                parser
--   <|>
--     fst <$> getState

-- arg :: ParsecT [String] u Identity String
-- arg = tokenPrim id (\sp t s -> sp) Just
  


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
