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
, getArea
) where



import Data.Maybe
import Go.Intersection



type Board = [[Intersection]]


createBoard :: Int -> Board
createBoard lines = (map . map) addEmptyIntersection board
  where board = map (zip [0..] . replicate lines) [0..(lines - 1)]



addEmptyIntersection :: (Int, Int) -> Intersection
addEmptyIntersection (x, y) = Intersection (Location x y) Empty



placeStone :: Board -> Location -> State -> Board
placeStone board location state = (map . map) (updateState location state) board



updateState :: Location -> State -> Intersection -> Intersection
updateState locationToUpdate newState (Intersection location state)
  | locationToUpdate == location = Intersection location newState
  | otherwise                    = Intersection location state



isLocationEmpty :: Board -> Location -> Bool
isLocationEmpty board (Location x y) = isEmpty (board!!y!!x)



removeStonesWithoutLiberty :: Board -> Color -> Board
removeStonesWithoutLiberty board color = (map . map) (removeStoneWithoutLiberty board color) board



removeStoneWithoutLiberty :: Board -> Color -> Intersection -> Intersection
removeStoneWithoutLiberty board color intersection
  | hasDifferentColor               = intersection
  | isEmpty intersection            = intersection
  | hasLiberty board [intersection] = intersection
  | otherwise                       = setEmpty intersection
  where hasDifferentColor           = not (hasColor intersection color)



hasLiberty :: Board -> [Intersection] -> Bool
hasLiberty board (intersection:connectedGroup)
  | hasIntersectionLiberty board intersection                          = True
  | hasAnyConnectedAdjacentLiberty board $ intersection:connectedGroup = True
  | otherwise                                                          = False



hasIntersectionLiberty :: Board -> Intersection -> Bool
hasIntersectionLiberty board intersection
  | hasAnyEmptyAdjacent     = True
  | otherwise               = False
  where hasAnyEmptyAdjacent = any (maybe False isEmpty) adjacents
        adjacents           = getAdjacents board intersection



hasAnyConnectedAdjacentLiberty :: Board -> [Intersection] -> Bool
hasAnyConnectedAdjacentLiberty board (intersection:connections) =
  any (maybe False $ hasConnectedAdjacentLiberty board connectedGroup) adjacents
  where connectedGroup = intersection:connections
        adjacents      = getAdjacents board intersection



hasConnectedAdjacentLiberty :: Board -> [Intersection] -> Intersection-> Bool
hasConnectedAdjacentLiberty board (adjacent:connections) intersection
  | hasDifferentState intersection = False
  | alreadyChecked intersection    = False
  | otherwise                      = hasLiberty board (intersection:adjacent:connections)
  where alreadyChecked             = includes (adjacent:connections)
        hasDifferentState          = (==) False . hasSameState adjacent



getArea :: Board -> Color -> [Intersection]
getArea board color = territory ++ occupiedIntersections
  where territory             = getTerritory board color
        occupiedIntersections = getOccupiedIntersections board color



getOccupiedIntersections :: Board -> Color -> [Intersection]
getOccupiedIntersections board color = filter (isOccupied color) (concat board)
  where isOccupied = flip hasColor



getTerritory :: Board -> Color -> [Intersection]
getTerritory board color = foldr (addTerritory board color) [] (concat board)



addTerritory :: Board -> Color -> Intersection -> [Intersection] -> [Intersection]
addTerritory board color intersection territory
  | isPrisoner           = territory
  | isEmpty intersection = territory ++ createTerritory board color intersection
  | otherwise            = territory
  where isPrisoner = includes territory intersection



createTerritory :: Board -> Color -> Intersection -> [Intersection]
createTerritory board color intersection = fromMaybe [] territory
  where territory = addAdjacentsToTerritory board color [intersection]



addAdjacentsToTerritory :: Board -> Color -> [Intersection] -> Maybe [Intersection]
addAdjacentsToTerritory board color territory =
  foldr (addAdjacentToTerritory board color) (Just territory) adjacents
  where adjacents = getAdjacents board (head territory)



addAdjacentToTerritory :: Board -> Color -> Maybe Intersection -> Maybe [Intersection] -> Maybe [Intersection]
addAdjacentToTerritory board color maybeAdjacent maybeTerritory
  | isNothing maybeAdjacent  = maybeTerritory
  | isNothing maybeTerritory = Nothing
  | isPrisoner               = maybeTerritory
  | hasSameColor             = maybeTerritory
  | isEmpty intersection     = addAdjacentsToTerritory board color (intersection:territory)
  | otherwise                = Nothing
  where isPrisoner   = includes territory intersection
        hasSameColor = hasColor intersection color
        intersection = fromJust maybeAdjacent
        territory    = fromJust maybeTerritory



getAdjacents :: Board -> Intersection -> [Maybe Intersection]
getAdjacents board (Intersection location state) = [top, right, bottom, left]
  where top    = getTopAdjacent board location
        right  = getRightAdjacent board location
        bottom = getBottomAdjacent board location
        left   = getLeftAdjacent board location



getTopAdjacent :: Board -> Location -> Maybe Intersection
getTopAdjacent board (Location x y)
  | isOutOfField     = Nothing
  | otherwise        = Just (board!!y!!(x - 1))
  where isOutOfField = (x - 1) < 0



getRightAdjacent :: Board -> Location -> Maybe Intersection
getRightAdjacent board (Location x y)
  | isOutOfField     = Nothing
  | otherwise        = Just (board!!(y + 1)!!x)
  where isOutOfField = (y + 1) >= length board



getBottomAdjacent :: Board -> Location -> Maybe Intersection
getBottomAdjacent board (Location x y)
  | isOutOfField     = Nothing
  | otherwise        = Just (board!!y!!(x + 1))
  where isOutOfField = (x + 1) >= length board



getLeftAdjacent :: Board -> Location -> Maybe Intersection
getLeftAdjacent board (Location x y)
  | isOutOfField     = Nothing
  | otherwise        = Just (board!!(y - 1)!!x)
  where isOutOfField = (y - 1) < 0
