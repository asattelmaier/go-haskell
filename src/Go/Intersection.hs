module Go.Intersection
( Location (Location)
, Color (Black, White)
, State (Empty, Stone)
, Intersection (Intersection)
, isEmpty
, setEmpty
, hasSameState
, hasColor
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



getState :: Intersection -> State
getState (Intersection location state) = state



hasSameState :: Intersection -> Intersection -> Bool
hasSameState intersectionA intersectionB = getState intersectionA == getState intersectionB



getColor :: Intersection -> Color
getColor (Intersection location (Stone color)) = color



hasColor :: Intersection -> Color -> Bool
hasColor intersection color
  | isEmpty intersection = False
  | otherwise            = getColor intersection == color



includes :: [Intersection] -> Intersection -> Bool
includes intersections intersection
  | isNothing result = False
  | otherwise        = True
  where result = find (intersection ==) intersections

