import Control.Applicative
import Control.Monad
import GroupOutput
import Language.MiniZinc
import Language.MiniZinc.Convert
import System.Environment

main = do
  a <- head <$> getArgs
  em <- readModelFile a
  case em of
    Left err -> error (show err)
    Right m -> do putStrLn =<< prettyPrintModel m
                  forM_ [ e | ConstraintI e <- _modelItems m ] $ print . freeIdentifiers

