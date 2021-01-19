module Go.Intersection
( Location (Location)
, Color (Black, White)
, State (Empty, Stone)
, Intersection (Intersection)
, isEmpty
, setEmpty
, hasSameColor
, includes
) where



import Data.List
import Data.Maybe



data Location     = Location Int Int deriving (Show, Eq)
data Color        = Black | White deriving (Show, Eq)
data State        = Empty | Stone Color deriving (Show, Eq)
data Intersection = Intersection Location State deriving (Show, Eq)



isEmpty :: Intersection -> Bool
isEmpty (Intersection location state) = state == Empty



setEmpty :: Intersection -> Intersection
setEmpty (Intersection location state) = Intersection location Empty



getColor :: Intersection -> State
getColor (Intersection location state) = state



hasSameColor :: Intersection -> Intersection -> Bool
hasSameColor intersectionA intersectionB = getColor intersectionA == getColor intersectionB



includes :: [Intersection] -> Intersection -> Bool
includes intersections intersection
  | isNothing result = False
  | otherwise        = True
  where result = find (intersection ==) intersections

