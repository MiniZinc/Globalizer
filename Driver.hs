{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.ParallelIO
import Control.Exception
import Control.Monad.State.Strict
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.IO.Handle
import System.GlobalLock
import System.IO
import System.Posix.Signals
import Text.Printf

import GroupOutput
import Statistics

-- Add the pair (file,ranges) to the map.
addToMap :: M.Map String (S.Set String) -> (String, String) -> M.Map String (S.Set String)
addToMap m (file,ranges) = M.insertWith S.union ranges (S.singleton file) m

main :: IO ()
main = do
  -- Read standard input, which should look something like this:
  --
  --   generated_subproblems/jobshop2x2/m1s0d0.mzn 36:5..49:5 /
  --   generated_subproblems/jobshop2x2/m1s2d0.mzn 36:5..49:5 / 36:5..49:5 / 36:5..49:5 /
  --   generated_subproblems/jobshop2x2/m1s7d0.mzn 44:23..44:29 44:23..44:29 46:6..47:53 /
  --   generated_subproblems/jobshop2x2/m1s8d0.mzn 44:23..44:29 44:23..44:29 46:6..47:53 /
  --
  -- Split each line into the filename (everything before the first
  -- space) and the ranges (the rest), and add that to the map.
  mapping <- foldl' addToMap M.empty . map (break isSpace) . lines <$> getContents

  -- Install handler for SIGINT.
  myThreadId >>= \tid ->
    void $ installHandler sigINT (Catch (hPutStrLn stderr "bye bye" >> killThread tid)) Nothing

  runGroups mapping

