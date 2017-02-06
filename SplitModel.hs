module SplitModel where

import Control.Applicative
import Control.Arrow (first)
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Language.MiniZinc.Types
import Language.MiniZinc.Parser
import System.Cmd
import System.Exit
import System.Directory
import System.IO
import System.IO.Silently
import System.IO.Temp

type ModelGroup = S.Set (Model ())

builtinsFile = "/home/chris/research/projects/global-constraints/subproblemsplitter/builtins.mzn"
minizincStdlib = "/opt/minizinc/minizinc-1.6/lib/minizinc/std"

splitModel :: FilePath -> [FilePath] -> IO (Either String (M.Map String ModelGroup))
splitModel mzn dzns = do
  finalResult <- try $ do
    -- Make canonical (absolute) versions of the file paths.
    canonicalMzn <- canonicalizePath mzn
    canonicalDzns <- mapM canonicalizePath dzns
    -- Store current directory.
    oldCurrentDirectory <- getCurrentDirectory
    -- Do our work in a temporary directory.
    result <- withSystemTempDirectory "globalizer" $ \tempDirPath -> runEitherT $ do
      -- Change to the temporary directory.
      lift $ setCurrentDirectory tempDirPath
      -- Sanity check files.
      maybe (return ()) left =<< lift (sanityCheck canonicalMzn canonicalDzns)
      -- Copy builtins.mzn to temporary directory.
      lift $ copyFile builtinsFile (tempDirPath ++ "/builtins.mzn")
      -- Call model splitter.
      (output,exitCode) <- lift $ hCapture [stdout] $
                             callModelSplitter canonicalMzn canonicalDzns
      when (exitCode /= ExitSuccess) (left ("call to SubproblemSplitter failed: " ++ output))
      -- Let "mapping" map each range-string to a set of files.
      let mapping = parseSplitterOutput output
      -- Parse the models that the splitter created.
      liftM M.fromList $ forM (M.toList mapping) $ \(range, files) -> do
        models <- liftM S.fromList $ forM (S.toList files) $ \file -> do
          -- If parsing succeeds, return the model.
          -- If parsing fails, return the error message.
          either (left . show) (right . stripDecorationModel) =<< lift (parseModelFile file)
        return (range, models)
    -- Go back to where we came from.
    setCurrentDirectory oldCurrentDirectory
    -- Return either the error message, or the list of model groups.
    return result
  case finalResult of
    -- We caught an IO exception.
    Left exception -> return (Left (show (exception :: IOException)))
    -- One of our own checks failed.
    Right (Left message) -> return (Left message)
    -- Everything went well.
    Right (Right list) -> return (Right list)

-- Parse the output from the subproblem splitter.
--
-- Example of subproblem splitter's output:
--
-- generated_subproblems/partition/m1s0d0.mzn 43:9..49:45 / 
-- generated_subproblems/partition/m1s1d0.mzn 43:9..49:45 / 40:9..40:35 / 
-- generated_subproblems/partition/m1s2d0.mzn 43:9..49:45 / 40:9..40:35 / 39:9..39:35 / 
--
-- The result is a map from the range string (the part after the
-- filename) to the set of files that have that range string.

parseSplitterOutput :: String -> M.Map String (S.Set FilePath)
parseSplitterOutput output =
  foldl' addToMap M.empty . map (break isSpace) . lines $ output
  where
    addToMap m (file,ranges) = M.insertWith S.union ranges (S.singleton file) m

-- Sanity check of the given files:
--  * they must be files
--  * they must have the right extension (.mzn / .dzn)
--
-- Returns an error message, or Nothing if there are no errors.
sanityCheck :: FilePath -> [FilePath] -> IO (Maybe String)
sanityCheck mzn dzns =
  let iochecks = [ (doesFileExist mzn, mzn ++ " is not a file") ]
            ++ map (\d -> (doesFileExist d, d ++ " is not a file")) dzns
      purechecks = [ (return (".mzn" `isSuffixOf` mzn), mzn ++ " must end in .mzn") ]
             ++ map (\d -> (return (".dzn" `isSuffixOf` d), d ++ " must end in .dzn")) dzns
  in fmap snd <$> firstM (liftM not . fst) (purechecks ++ iochecks)

-- Call the model splitter.
callModelSplitter :: FilePath -> [FilePath] -> IO ExitCode
callModelSplitter mzn dzns = do
  rawSystem "SubproblemSplitter" $    ["-I", minizincStdlib]
                                   ++ ["-M", "2"]
                                   ++ (mzn:dzns)
