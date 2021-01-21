module Go.Board
( Board
, State (Empty, Stone)
, Color (Black, White)
, Location (Location)
, Intersection (Intersection)
, createBoard
, placeStone
, isLocationEmpty
, removeStonesWithoutLiberty
) where



import Data.Maybe
import Go.Intersection



type Board = [[Intersection]]


createBoard :: Int -> Board
createBoard lines = (map . map) addEmptyIntersection board
  where board = map (flip zip [0..] . replicate lines) [0..(lines - 1)]



addEmptyIntersection :: (Int, Int) -> Intersection
addEmptyIntersection (x, y) = Intersection (Location x y) Empty



placeStone :: Board -> Location -> State -> Board
placeStone board location state = (map . map) (updateState location state) board



updateState :: Location -> State -> Intersection -> Intersection
updateState locationToUpdate newState (Intersection location state)
  | locationToUpdate == location = Intersection location newState
  | otherwise                    = Intersection location state



isLocationEmpty :: Board -> Location -> Bool
isLocationEmpty board (Location x y) = isEmpty (board!!x!!y)



removeStonesWithoutLiberty :: Board -> Color -> Board
removeStonesWithoutLiberty board color = (map . map) (removeStoneWithoutLiberty board color) board



removeStoneWithoutLiberty :: Board -> Color -> Intersection -> Intersection
removeStoneWithoutLiberty board color intersection
  | hasDifferentColor               = intersection
  | isEmpty intersection            = intersection
  | hasLiberty board [intersection] = intersection
  | otherwise                       = setEmpty intersection
  where hasDifferentColor           = hasColor intersection color == False



hasLiberty :: Board -> [Intersection] -> Bool
hasLiberty board (intersection:connectedGroup)
  | hasIntersectionLiberty board intersection                          = True
  | hasAnyConnectedAdjacentLiberty board $ intersection:connectedGroup = True
  | otherwise                                                          = False



hasIntersectionLiberty :: Board -> Intersection -> Bool
hasIntersectionLiberty board (Intersection location state)
  | hasAnyEmptyAdjacent     = True
  | otherwise               = False
  where hasAnyEmptyAdjacent = any (maybe False isEmpty) adjacents
        adjacents           = getAdjacents board location



hasAnyConnectedAdjacentLiberty :: Board -> [Intersection] -> Bool
hasAnyConnectedAdjacentLiberty board ((Intersection location state):connections) =
  any (maybe False $ hasConnectedAdjacentLiberty board connectedGroup) adjacents
  where connectedGroup = intersection:connections
        intersection   = Intersection location state
        adjacents      = getAdjacents board location



hasConnectedAdjacentLiberty :: Board -> [Intersection] -> Intersection-> Bool
hasConnectedAdjacentLiberty board (adjacent:connections) intersection
  | hasDifferentState intersection = False
  | alreadyChecked intersection    = False
  | otherwise                      = hasLiberty board (intersection:adjacent:connections)
  where alreadyChecked             = includes (adjacent:connections)
        hasDifferentState          = (==) False . hasSameState adjacent



getAdjacents :: Board -> Location -> [Maybe Intersection]
getAdjacents board location = [top, right, bottom, left]
  where top    = getTopAdjacent board location
        right  = getRightAdjacent board location
        bottom = getBottomAdjacent board location
        left   = getLeftAdjacent board location



getTopAdjacent :: Board -> Location -> Maybe Intersection
getTopAdjacent board (Location x y)
  | isOutOfField     = Nothing
  | otherwise        = Just (board!!x!!(y - 1))
  where isOutOfField = (y - 1) < 0



getRightAdjacent :: Board -> Location -> Maybe Intersection
getRightAdjacent board (Location x y)
  | isOutOfField     = Nothing
  | otherwise        = Just (board!!(x + 1)!!y)
  where isOutOfField = (x + 1) >= length board



getBottomAdjacent :: Board -> Location -> Maybe Intersection
getBottomAdjacent board (Location x y)
  | isOutOfField     = Nothing
  | otherwise        = Just (board!!x!!(y + 1))
  where isOutOfField = (y + 1) >= length board



getLeftAdjacent :: Board -> Location -> Maybe Intersection
getLeftAdjacent board (Location x y)
  | isOutOfField     = Nothing
  | otherwise        = Just (board!!(x - 1)!!y)
  where isOutOfField = (x - 1) < 0
