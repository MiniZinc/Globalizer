{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Loc where

import Control.Lens
import Data.Data
import Data.List
import qualified Data.Text as T 
import Data.Monoid
import Data.Ord
import Language.MiniZinc
import Text.Printf

modelConstraintLocations :: Model -> DisjointLocation
modelConstraintLocations m = locComb $ do
  ConstraintI e <- m ^. modelItems
  map (\l -> DisjointLocation [l]) $ e ^.. visitLocationsExp

expConstraintLocations :: Expression -> DisjointLocation
expConstraintLocations e =
  locComb $
    map (\l -> DisjointLocation [l]) $ e ^.. visitLocationsExp

class Loc a where
    locCombine :: [a] -> a

instance Loc Location where
    locCombine = foldr locPlus mempty

locPlus :: Location -> Location -> Location
locPlus l1 l2 = Location (posMin (startPos l1) (startPos l2))
                         (posMax (endPos l1) (endPos l2))

posMin :: Position -> Position -> Position
posMin p1 p2 = minimumBy (comparing posLine <> comparing posColumn) [p1,p2]

posMax :: Position -> Position -> Position
posMax p1 p2 = maximumBy (comparing posLine <> comparing posColumn) [p1,p2]

instance Loc () where
    locCombine _ = ()

instance Loc DisjointLocation where
    locCombine = condenseDisjointLocation . DisjointLocation . concat . map unDisjointLocation

locComb :: [DisjointLocation] -> DisjointLocation
locComb = condenseDisjointLocation . DisjointLocation . concat . map unDisjointLocation

newtype DisjointLocation = DisjointLocation { unDisjointLocation :: [Location] }
  deriving (Semigroup, Monoid, Show, Typeable, Data, Eq, Ord)

locExp :: Expression' -> Expression
locExp e' = mkExp e'

condenseDisjointLocation :: DisjointLocation -> DisjointLocation
condenseDisjointLocation (DisjointLocation locs) = DisjointLocation $
    filter (\l -> not (any (\l2 -> l /= l2 && l `locationInside` l2) locs))
    . nub
    $ locs

locationInside :: Location -> Location -> Bool
locationInside (Location s1 e1) (Location s2 e2) =
    s1 `positionAfter` s2 && e2 `positionAfter` e1

positionAfter :: Position -> Position -> Bool
positionAfter p1 p2 =
    posName p1 == posName p2
    && (posLine p1, posColumn p1) >= (posLine p2, posColumn p2)

showDisjointLocation :: String -> DisjointLocation -> String
showDisjointLocation modelFile (DisjointLocation locs) =
    intercalate ";" (map f (sort locs))
  where
    f (Location (Position _ l1 c1) (Position _ l2 c2)) =
        printf "%s|%d|%d|%d|%d" modelFile l1 c1 l2 c2

split :: String -> String -> [String]
split sep s = map T.unpack (T.splitOn (T.pack sep) (T.pack s))

splitPaths :: String -> [String]
splitPaths paths = split ";" paths

splitPath :: String -> [String]
splitPath path = split "|" path

v4tot4 :: [a] -> (a,a,a,a)
v4tot4 [sl,sc,el,ec] = (sl,sc,el,ec)
v4tot4 _ = error "Not enough elements in list"

pathToTuples :: String -> [(Int, Int, Int, Int)]
pathToTuples paths = [ v4tot4 [ read i :: Int | i <- splitPath path ] | path <- splitPaths paths ]

pathsToDisjointLocation :: String -> DisjointLocation
pathsToDisjointLocation paths = DisjointLocation (map (\(sl,sc,el,ec) -> Location (Position "input" sl sc)
                                                                                  (Position "input" el ec))
                                                      (pathToTuples paths))

showExpLocation :: String -> Expression -> String
showExpLocation modelFile e =
  case e ^. expLocation of
    Just l -> showDisjointLocation modelFile (DisjointLocation [l])
    Nothing -> showDisjointLocation modelFile (expConstraintLocations e)


