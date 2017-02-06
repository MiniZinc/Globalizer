{-# LANGUAGE OverloadedStrings #-}

import Statistics
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.State.Strict

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
  stats <- flip execStateT emptyStatistics $ statisticsTime "outer" $ do
             y <- statisticsTime "middle" $ do
                    x <- statisticsTime "inner" $ liftIO $ ((try (return $! fib 36)) :: IO (Either IOException Integer))
                    let r = case x of
                              Left _ -> 0
                              Right z -> z
                    return $! r
             liftIO $ print y
  putStrLn (showStatistics stats)
