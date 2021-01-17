module Board
( Board
, State (Empty, Stone)
, Color (Black, White)
, Intersection (Intersection)
, createBoard
, placeStone
, isLocationEmpty
, removeStonesWithoutLiberties
) where



import Data.List
import Data.Maybe
import Location (Location (Location))



data Color        = Black | White deriving (Show, Eq)
data State        = Empty | Stone Color deriving (Show, Eq)
data Intersection = Intersection Location State deriving (Show, Eq)
type Board        = [[Intersection]]



createBoard :: Int -> Board
createBoard lines = createIntersections (Location lines lines)



createIntersections :: Location -> [[Intersection]]
createIntersections (Location 0 y) = []
createIntersections (Location x y) = createIntersections (Location (x - 1) y) ++ [horizontalLine]
   where horizontalLine = createHorizontalLine (Location x y)



createHorizontalLine :: Location -> [Intersection]
createHorizontalLine (Location x 0)    = []
createHorizontalLine (Location x y)= createHorizontalLine (Location x (y - 1)) ++ [intersection]
  where intersection = Intersection (Location (x - 1) (y - 1)) Empty




placeStone :: Board -> Location -> State -> Board
placeStone board location state = (map . map) (updateState location state) board



updateState :: Location -> State -> Intersection -> Intersection
updateState locationToUpdate newState (Intersection location state)
  | locationToUpdate == location = Intersection location newState
  | otherwise                    = Intersection location state



isLocationEmpty :: Board -> Location -> Bool
isLocationEmpty board (Location x y) = isEmpty (board!!x!!y)



-- TODO: Create Intersection Module
isEmpty :: Intersection -> Bool
isEmpty (Intersection location state) = state == Empty



setEmpty :: Intersection -> Intersection
setEmpty (Intersection location state) = Intersection location Empty



removeStonesWithoutLiberties :: Board -> Board
removeStonesWithoutLiberties board = (map . map) (handleStoneRemoval board) board



handleStoneRemoval :: Board -> Intersection -> Intersection
handleStoneRemoval board intersection
  | isEmpty intersection                      = intersection
  | hasConnectionLiberty board [intersection] = intersection
  | otherwise                                 = setEmpty intersection



hasConnectionLiberty :: Board -> [Intersection] -> Bool
hasConnectionLiberty board ((Intersection (Location x y) state):connections)
  | hasLiberty board intersection           = True
  | hasSomeConnectionLiberty                = True
  | otherwise                               = False
  where hasSomeConnectionLiberty            = hasTopLiberty || hasRightLiberty || hasBottomLiberty || hasLeftLiberty
        intersection                        = Intersection (Location x y) state
        hasTopLiberty    
          | (y - 1) < 0                     = False
          | hasTopSameColor == False        = False
          | includes connections top        = False
          | otherwise                       = hasConnectionLiberty board (top:intersection:connections)
          where hasTopSameColor             = hasSameColor intersection top
-- TODO: Reuse code
                top                         = board!!x!!(y - 1)
        hasRightLiberty    
          | (x + 1) >= length board         = False
          | hasRightSameColor == False      = False
          | includes connections right      = False
          | otherwise                       = hasConnectionLiberty board (right:intersection:connections)
          where hasRightSameColor           = hasSameColor intersection right
                right                       = board!!(x + 1)!!y
        hasBottomLiberty    
          | (y + 1) >= length board         = False
          | hasBottomSameColor == False     = False 
          | includes connections bottom     = False
          | otherwise                       = hasConnectionLiberty board (bottom:intersection:connections)
          where hasBottomSameColor          = hasSameColor intersection bottom
                bottom                      = board!!x!!(y + 1)
        hasLeftLiberty    
          | (x - 1) < 0                     = False
          | hasLeftSameColor == False       = False 
          | includes connections left       = False
          | otherwise                       = hasConnectionLiberty board (left:intersection:connections)
          where hasLeftSameColor            = hasSameColor intersection left
                left                        = board!!(x - 1)!!y



includes :: [Intersection] -> Intersection -> Bool
includes intersections intersection
  | isNothing result = False
  | otherwise        = True
  where result = find ((==) intersection) intersections



hasSameColor :: Intersection -> Intersection -> Bool
hasSameColor (Intersection locationA (Stone colorA)) (Intersection locationB (Stone colorB)) =
  colorA == colorB



hasLiberty :: Board -> Intersection -> Bool
hasLiberty board (Intersection (Location x y) state)
  | hasEmptyAdjacent                = True
  | otherwise                       = False
  where hasEmptyAdjacent            = isTopEmpty || isRightEmpty || isBottomEmpty || isLeftEmpty
        isTopEmpty    
          | (y - 1) < 0             = False
          | otherwise               = isEmpty $ board!!x!!(y - 1)
        isRightEmpty
          | (x + 1) >= length board = False
          | otherwise               = isEmpty $ board!!(x + 1)!!y
        isBottomEmpty
          | (y + 1) >= length board = False
          | otherwise               = isEmpty $ board!!x!!(y + 1)
        isLeftEmpty   
          | (x - 1) < 0             = False
          | otherwise               = isEmpty $ board!!(x - 1)!!y
        intersection                = Intersection (Location x y) state

