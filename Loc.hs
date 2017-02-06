{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Loc where

import Control.Lens
import Data.Data
import Data.Data.Lens
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Language.MiniZinc
import Text.Printf


modelConstraintLocations :: Model -> DisjointLocation
modelConstraintLocations m = locComb $ do
  ConstraintI e <- m ^. modelItems
--  map (\l -> DisjointLocation [l]) $ catMaybes $ map _expLocation (universeOf subExpressions e)
  map (\l -> DisjointLocation [l]) $ e ^.. visitLocationsExp
--  concat ( e ^.. subExpressions )

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
--    locCombine = DisjointLocation . concat . map unDisjointLocation

locComb = condenseDisjointLocation . DisjointLocation . concat . map unDisjointLocation

newtype DisjointLocation = DisjointLocation { unDisjointLocation :: [Location] }
  deriving (Monoid, Show, Typeable, Data, Eq, Ord)

locExp :: Expression' -> Expression
locExp e' = mkExp e'
-- { _expDecoration = ()
--                   , _expAnnotations = mempty
--                   , _expRawExpression = e'
--                   , _expLocation = Nothing
-- case mkExp e' ^.. subExpressions of
--                                           [] -> Nothing
--                                           es -> Just $ locComb $
--                                                   map (\l -> DisjointLocation [l]) $ catMaybes $ map _expLocation es
--                       }

-- makeDisjointLocation :: Model -> Model DisjointLocation
-- makeDisjointLocation = over mapped (\l -> DisjointLocation [l])

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
              
showDisjointLocation :: DisjointLocation -> String
showDisjointLocation (DisjointLocation locs) =
    unwords (map f (sort locs))
  where
    f (Location (Position _ l1 c1) (Position _ l2 c2)) =
        printf "%d:%d..%d:%d" l1 c1 l2 c2

showExpLocation :: Expression -> String
showExpLocation e =
  case e ^. expLocation of
    Just l -> showDisjointLocation (DisjointLocation [l])
    Nothing -> showDisjointLocation (expConstraintLocations e)


