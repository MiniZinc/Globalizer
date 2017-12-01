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
                       , freeSearch :: Bool
                       }
  deriving (Show)

withInfo :: O.Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseOptions :: O.Parser GlobalizerOptions
parseOptions = GlobalizerOptions
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
                 <*> parseFreeSearch

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
  <> help "Consider only constraints containing these comma seprated substrings"

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

parseFreeSearch :: O.Parser Bool
parseFreeSearch = flag False True $
  long "free-search"
  <> help "Don't use random search heuristics"
  <> showDefault


