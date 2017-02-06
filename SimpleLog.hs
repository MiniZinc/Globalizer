{-# LANGUAGE RankNTypes #-}
module SimpleLog where

import System.IO as IO
import Control.Monad
import Control.Monad.IO.Class

data LogCategory = LogDebug
                 | LogVariables
                 | LogArgs
                 | LogSolving
                 | LogChecking
                 | LogModel
                 | LogHigh
                 | LogScoring
                 | LogConstraints
                 | LogNormalisation
  deriving (Eq, Show)

data Handle = Handle {
  log :: forall m. MonadIO m => LogCategory -> String -> m ()
, logN :: forall m. MonadIO m => LogCategory -> String -> m ()
, logPrint :: forall a m. (Show a, MonadIO m) => LogCategory -> a -> m ()
}

newHandle :: [LogCategory] -> IO.Handle -> IO SimpleLog.Handle
newHandle [] _ = return $ SimpleLog.Handle {
  SimpleLog.log = \cat msg -> return ()
, SimpleLog.logN = \cat msg -> return ()
, SimpleLog.logPrint = \cat msg -> return ()
}
newHandle categories h = do
  return $ SimpleLog.Handle {
    SimpleLog.log = \cat msg ->
      when (cat `elem` categories) $ do
        liftIO $ hPutStrLn h msg
        liftIO $ hFlush h
  , SimpleLog.logN = \cat msg ->
      when (cat `elem` categories) $
        liftIO $ do hPutStr h msg
                    hFlush h
  , SimpleLog.logPrint = \cat x ->
      when (cat `elem` categories) $ do
        liftIO $ hPrint h x
        liftIO $ hFlush h
  }
