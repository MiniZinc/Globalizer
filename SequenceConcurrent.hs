module SequenceConcurrent where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Exception

data Output a = Result a
              | Done

worker actionChannel outputChannel = loop
  where loop = do
          maybeAction <- atomically $ tryReadTChan actionChannel
          case maybeAction of
            Nothing -> atomically $ writeTChan outputChannel Done
            Just action -> do
                 result <- action
                 evaluate result
                 atomically $ writeTChan outputChannel (Result result)
                 loop

sequenceConcurrent :: Int -> [IO a] -> IO [a]
sequenceConcurrent nworkers actions = do
  actionChannel <- newTChanIO
  outputChannel <- newTChanIO
  forM_ actions $ atomically . writeTChan actionChannel
  replicateM_ nworkers (forkIO (worker actionChannel outputChannel))
  let loop acc 0 = return (reverse acc)
      loop acc nwaiting = do
         output <- atomically $ readTChan outputChannel
         case output of
           Done -> loop acc (nwaiting-1)
           Result r -> loop (r:acc) nwaiting
  loop [] nworkers

forMConcurrent :: Int -> [a] -> (a -> IO b) -> IO [b]
forMConcurrent nworkers inputs f = sequenceConcurrent nworkers (map f inputs)
