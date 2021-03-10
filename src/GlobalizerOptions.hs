{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module GlobalizerOptions where

import Control.Applicative
import Data.Maybe
import Data.Semigroup ((<>))

import Options.Applicative as O
import SimpleLog

data GlobalizerOptions = GlobalizerOptions { inputFiles :: [String]
                       , maxConstraints :: Int
                       , nRandomSolutions :: Int
                       , nSampleSolutions :: Int
                       , debugging :: [LogCategory]
                       , stdlibDir :: Maybe String
                       , minizincPath :: Maybe String
                       , constraintFilterIn :: Maybe String
                       , constraintFilterEx :: Maybe String
                       , solvingTimeout :: Integer
                       , selectGroup :: Maybe Int
                       , includePaths :: Maybe String
                       , excludePaths :: Maybe String
                       , numJobs :: Int
                       , filterArguments :: Bool
                       , maxArguments :: Int
                       , doHandleITEForalls :: Bool
                       , doHandleForallITEs :: Bool
                       , doInitialPass :: Bool
                       , doImpliesCheck :: Bool
                       , doOutputHTML :: Bool
                       , freeSearch :: Bool
                       }
  deriving (Show)

withInfo :: O.Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseOptions :: O.Parser GlobalizerOptions
parseOptions = GlobalizerOptions
                 <$> parseArguments
                 <*> parseMaxConstraints
                 <*> parseRandomSolutions
                 <*> parseSampleSolutions
                 <*> parseDebugging
                 <*> parseStdlibDir
                 <*> parseMinizincPath
                 <*> parseConstraintFilterIn
                 <*> parseConstraintFilterEx
                 <*> parseSolvingTimeout
                 <*> parseSelectGroup
                 <*> parseIncludePaths
                 <*> parseExcludePaths
                 <*> parseNumJobs
                 <*> parseFilterArguments
                 <*> parseMaxArguments
                 <*> parseDoHandleITEForalls
                 <*> parseDoHandleForallITEs
                 <*> parseDoInitialPass
                 <*> parseDoImpliesCheck
                 <*> parseDoOutputHTML
                 <*> parseFreeSearch

parseIncludePaths :: O.Parser (Maybe String)
parseIncludePaths = optional $ option str $
  long "includePaths"
  <> metavar "<space separated line numbers>"
  <> help "Consider only constraints within the path spans provided. \"sl|sc|el|ec;sl|sc|el|ec\" "

parseExcludePaths :: O.Parser (Maybe String)
parseExcludePaths = optional $ option str $
  long "excludePaths"
  <> metavar "<space separated line numbers>"
  <> help "Ignore constraints within the path spans provided. \"sl|sc|el|ec;sl|sc|el|ec\" "

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
  help "Input files (.mzn and .dzn)"
  <> metavar "<input files>"

parseDebugging :: O.Parser [LogCategory]
parseDebugging =
  let list :: [O.Parser (Maybe LogCategory)]
      list = do (category, opt, desc) <- cats
                return $ O.flag Nothing (Just category) $
                  (long ("debug-"++opt) <> help ("Produce debugging output related to "++desc))
   in
   let verb :: O.Parser (Maybe LogCategory)
       verb = O.flag Nothing (Just LogHigh) $
                  (short 'v' <> help "Verbose output (same as --debug-high)")
  in catMaybes <$> (sequenceA  $ (verb: list))
  where
    cats = [ (LogArgs, "args", "argument generation")
           , (LogConstraints, "constraints", "constraint checking")
           , (LogScoring, "scoring", "constraint scoring")
           , (LogChecking, "checking", "constraint checking")
           , (LogNormalisation, "norm", "model normalisation")
           , (LogSolving, "solving", "model solving")
           , (LogModel, "model", "group models")
           , (LogHigh, "high", "high-level progress") ]

parseConstraintFilterIn :: O.Parser (Maybe String)
parseConstraintFilterIn = optional $ option str $
  short 'f' <> long "constraintFilterIn"
  <> metavar "<substring>"
  <> help "Consider only constraints containing these comma seprated substrings"

parseConstraintFilterEx :: O.Parser (Maybe String)
parseConstraintFilterEx = optional $ option str $
  short 'e' <> long "constraintFilterEx"
  <> metavar "<substring>"
  <> help "Do not consider only constraints containing these comma seprated substrings"

parseStdlibDir :: O.Parser (Maybe String)
parseStdlibDir = optional $ option str $
  short 'I' <> long "stdlib-dir" <> long "mzn-stdlib-dir"
  <> metavar "<dir>"
  <> help "Location of MiniZinc stdlib directory. (This should contain \"globalizer\" directory)"

parseMinizincPath :: O.Parser (Maybe String)
parseMinizincPath = optional $ option str $
  long "minizinc-exe" <> long "mzn-exe"
  <> metavar "<dir>"
  <> help "Path to minizinc.exe if it is not not the system path."

parseSolvingTimeout :: O.Parser Integer
parseSolvingTimeout = option auto $
  short 't' <> long "solvingTimeout"
  <> metavar "<time>"
  <> help "Timeout for solving (in milliseconds)"
  <> value 1000
  <> showDefault

parseNumJobs :: O.Parser Int
parseNumJobs = option auto $
  short 'p' <> long "jobs" <> long "parallel"
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

parseDoHandleITEForalls :: O.Parser Bool
parseDoHandleITEForalls = flag False True $
  long "norm-if"
  <> help "Replace if statements containing foralls with foralls with where clauses"
  <> showDefault

parseDoHandleForallITEs :: O.Parser Bool
parseDoHandleForallITEs = flag False True $
  long "norm-fi"
  <> help "Replace foralls containing if statements with foralls with where clauses"
  <> showDefault

parseDoOutputHTML :: O.Parser Bool
parseDoOutputHTML = flag False True $
  long "output-html"
  <> help "Output HTML for MiniZincIDE"
  <> showDefault

parseFreeSearch :: O.Parser Bool
parseFreeSearch = flag False True $
  long "free-search"
  <> help "Don't use random search heuristics"
  <> showDefault


