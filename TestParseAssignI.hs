{-# LANGUAGE BangPatterns, DoAndIfThenElse #-}


import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.DeepSeq
import ParseAssignI
import GHC.AssertNF
import Data.List
import GHC.HeapView

-- main = print (parseAssignI "isGuest = array3d(1..3, 1..3, 1..3, [0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1]);")

parseBigFile fileContents =
  let ls = lines fileContents
      assigns = map (either (error "parse") id . parseAssignI) . filter (not . null) $ ls
  in assigns

fib x | x < 2 = x
      | otherwise = fib (x-1) + fib (x-2)

main = do
  c <- readFile "big-assigns.txt"
  let i = parseBigFile c
  print $ length i
  return $! length (show i)
  isNF i >>= \b -> unless b $ error "not in NF"






isNF x = isNFBoxed (asBox x)

isNFBoxed :: Box -> IO Bool
isNFBoxed b = do
    c <- getBoxedClosureData b
    nf <- isHNF c
    if nf
    then do
        c' <- getBoxedClosureData b
        allM isNFBoxed (allPtrs c')
    else do
        return False

-- Everything is in normal form, unless it is a
-- thunks explicitly marked as such.
-- Indirection are also considered to be in HNF
isHNF :: Closure -> IO Bool
isHNF c = do
    case c of
        ThunkClosure {}    -> return False 
        APClosure {}       -> return False
        SelectorClosure {} -> return False
        BCOClosure {}      -> return False
        _                  -> return True

