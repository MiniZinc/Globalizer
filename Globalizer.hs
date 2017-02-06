{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Language.MiniZinc.Types
import Language.MiniZinc.Print
import System.Console.CmdArgs
import System.IO
import Text.Printf

import GroupOutput
import SplitModel
import Statistics

data Args = Args {
      modelFile :: FilePath
    , dataFiles :: [FilePath]
    }
  deriving (Show, Data, Typeable)

globalizerArgs =
    Args { modelFile = def &= argPos 0 &= typ "<model.mzn>"
         , dataFiles = def &= args &= typ "<data.dzn>"
         } &= summary "MiniZinc globalizer, version 0"

main = do
  a <- cmdArgs globalizerArgs

  -- Call the model splitter.
  eitherModelGroups <- splitModel (modelFile a) (dataFiles a)
  case eitherModelGroups of
    Left message -> hPutStrLn stderr $ "there was a problem: " ++ message
    Right groups -> do print "process groups now"
                       forM_ (M.elems groups) $ \g -> do
                           putStrLn ""
                           putStrLn ""
                           print "start of group"
                           forM_ (S.toList g) $ \m -> do
                             putStrLn ""
                             print "start of model"
                             putStrLn $ plainShow m

                       (guidoParts,combinedStats) <- runGroupsModels groups
                       -- Glue all the output together.
                       let totalOutput = (concatMap unlines guidoParts)
                           ngroups = M.size groups
                       hPutStrLn stderr "START TOTAL"
                       hPutStrLn stderr totalOutput
                       hPutStrLn stderr "STOP TOTAL"
                       -- Write the output to a file.
--                       appendFile (printf "output/%s/guido-output" strippedName) totalOutput
                         
                       -- Print the statistics.
                       let statsString = "STATISTICS\n" ++ show combinedStats ++ "\n"
                           otherInfoString = "number of groups: " ++ show ngroups ++ "\n"
--                       hPutStrLn stderr (statsString ++ otherInfoString)
--                       appendFile (printf "output/%s/guido-output" strippedName) (statsString ++ otherInfoString)
                       hPutStrLn stderr (showStatistics combinedStats)

--                       forM_ (M.elems groups) $ \g -> do
--                         (out,st) <- 
  
  return ()
  
