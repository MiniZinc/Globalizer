{-# LANGUAGE DeriveDataTypeable #-}

import qualified Data.ByteString as BS
import qualified Data.Map as M
import Control.Applicative
import qualified Data.Set as S
import Data.Maybe
import Data.Serialize
import Control.Monad
import Language.MiniZinc
import Language.MiniZinc.Print
import System.Console.CmdArgs
import System.IO
import GroupOutput
--import IPPrint

-- import SplitModel

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

  globalsModel <- (either (const (error "no both-glob.mzn?")) id) <$> parseModelFile "both-glob.mzn"
  
  m <- fmap (fmap stripDecorationModel) $ parseModelFile (modelFile a)
  
  let env = topLevelBindings (stripDecorationModel globalsModel)

--  print m
  -- Call the model splitter.
--  eitherModelGroups <- splitModel (modelFile a) (dataFiles a)
  case m of
    Left message -> hPutStrLn stderr $ "there was a problem: " ++ show message
    Right m' -> do (o,s,l) <- runGroupsModels env (M.fromList [("only", S.singleton m')])
                   putStrLn (concatMap unlines o)
                   BS.writeFile "log" (encode l)
  
  return ()
  
