module Misc where

import Control.Monad (guard)
import Data.List (partition)

-- |Match elements of one list with the elements of another list,
-- satisfying compatibility between matched elements.  The lists must
-- have the same length.  Gives a list of all possible matchings.
-- 
-- mapM_ print $ pairUp (\x y -> abs (round x - y) <= 1) [1.2, 2.3, 2.7, 3.2] [1,2,3,4]
-- [(1.2,1),(2.3,2),(2.7,3),(3.2,4)]
-- [(1.2,1),(2.3,2),(2.7,4),(3.2,3)]
-- [(1.2,1),(2.3,3),(2.7,2),(3.2,4)]
-- [(1.2,1),(2.3,3),(2.7,4),(3.2,2)]
-- [(1.2,2),(2.3,1),(2.7,3),(3.2,4)]
-- [(1.2,2),(2.3,1),(2.7,4),(3.2,3)]
pairUp :: (a -> b -> Bool) -> [a] -> [b] -> [[(a,b)]]
pairUp _ [] [] = return []
pairUp compatible (a:as) bs = do
  -- Pick a "b" to be the partner of "a".
  (b,restb) <- select bs
  -- Ensure their compatibility.
  guard $ compatible a b
  -- Match up the rest of the "as" with the rest of the "bs".
  rest <- pairUp compatible as restb
  -- Return this matching.
  return $ (a,b):rest

-- |Choose an element from a list, returning a pair of the element and
-- the remainder of the list.  Gives a list of all possible choices.
-- (Similar to \"select\" in logic programming.)
-- 
-- e.g. select "abcd" ==> [('a',"bcd"),('b',"acd"),('c',"abd"),('d',"abc")]

-- This is the "pseudo-code" implementation that shows how it works.
-- select :: [a] -> [ (a, [a]) ]
-- select [] = []
-- select (x:xs) = do
--   yes <- [True,False]
--   if yes then return (x,xs)
--          else do (y,rest) <- select xs
--                  return $ (y,x:rest)

-- This is the fast, memory-efficient version.
select [] = []
select (x:xs) = (x,xs) : do (y,rest) <- select xs
                            return (y, x:rest)

-- Partition a list into equivalence classes by some equivalence
-- relation.
--
-- e.g.
--
-- partitionBy (\x y -> x `mod` 5 == y `mod` 5) [1..20]
--   ==> [[1,6,11,16],[2,7,12,17],[3,8,13,18],[4,9,14,19],[5,10,15,20]]
partitionBy :: (a -> a -> Bool) -> [a] -> [[a]]
partitionBy _  []     = []
partitionBy eq (v:vs) = let (x,y) = partition (v `eq`) (v:vs)
                        in x:(partitionBy eq y)

