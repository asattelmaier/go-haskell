module Go.Board
( Board
, State (Empty, Stone)
, Color (Black, White)
, Location (Location)
, Intersection (Intersection)
, createBoard
, placeStone
, isEmpty
, removeStonesWithoutLiberty
, getArea
) where



import           Control.Lens    (element, ix, (&), (.~), (^?))
import           Control.Monad   (join, liftM2)
import           Data.Maybe      (fromJust, fromMaybe, isNothing)
import           Go.Color        (Color (Black, White))
import           Go.Intersection (Intersection (Intersection),
                                  State (Empty, Stone), getX, getY, hasColor,
                                  hasSameState, includes, setEmpty)
import qualified Go.Intersection as Intersection (create, isEmpty)
import           Go.Location     (Location (Location))
import qualified Go.Location     as Location (getX, getY)



type Board = [[Intersection]]



createBoard :: Int -> Board
createBoard size = (map . map) addEmptyIntersection board
  where board = map (zip [0..] . replicate size) [0..(size - 1)]



addEmptyIntersection :: (Int, Int) -> Intersection
addEmptyIntersection (x, y) = Intersection (Location x y) Empty



placeStone :: Board -> Color -> Location -> Board
placeStone board color location = setIntersection board intersection
  where intersection = Intersection.create location color



setIntersection :: Board -> Intersection -> Board
setIntersection board intersection = board & ix y .~ setInRow
  where setInRow = (board!!y) & ix x .~ intersection
        x        = getX intersection
        y        = getY intersection



isEmpty :: Board -> Location -> Bool
isEmpty board location = maybe True Intersection.isEmpty intersection
  where intersection = getIntersection x y board
        x            = Location.getX location
        y            = Location.getY location



removeStonesWithoutLiberty :: Board -> Color -> Board
removeStonesWithoutLiberty board color = (map . map)
  (removeStoneWithoutLiberty board color) board



removeStoneWithoutLiberty :: Board -> Color -> Intersection -> Intersection
removeStoneWithoutLiberty board color intersection
  | shouldBeRemoved     = setEmpty intersection
  | otherwise           = intersection
  where shouldBeRemoved = shouldStoneBeRemoved board color intersection



shouldStoneBeRemoved :: Board -> Color -> Intersection -> Bool
shouldStoneBeRemoved board color intersection =
     not hasDifferentColor
  && not isEmptyIntersection
  && not hasIntersectionLiberty
  where hasDifferentColor      = not (hasColor intersection color)
        isEmptyIntersection    = Intersection.isEmpty intersection
        hasIntersectionLiberty = hasLiberty board [intersection]



hasLiberty :: Board -> [Intersection] -> Bool
hasLiberty board group =
     hasAnyEmptyAdjacent board (head group)
  || hasAnyConnectedAdjacentLiberty board group



hasAnyEmptyAdjacent :: Board -> Intersection -> Bool
hasAnyEmptyAdjacent = (any (maybe False Intersection.isEmpty) .) . getAdjacents 



hasAnyConnectedAdjacentLiberty :: Board -> [Intersection] -> Bool
hasAnyConnectedAdjacentLiberty board (intersection:connections) =
  any (maybe False $ hasConnectedAdjacentLiberty board connectedGroup) adjacents
  where connectedGroup = intersection:connections
        adjacents      = getAdjacents board intersection



hasConnectedAdjacentLiberty :: Board -> [Intersection] -> Intersection-> Bool
hasConnectedAdjacentLiberty board (adjacent:connections) intersection =
     not (alreadyChecked intersection)
  && hasSameState intersection adjacent
  && hasLiberty board (intersection:adjacent:connections)
  where alreadyChecked = includes (adjacent:connections)



getArea :: Board -> Color -> [Intersection]
getArea board color
  | hasPlacedAStone           = territory ++ occupiedIntersections
  | otherwise                 = []
  where territory             = getTerritory board color
        hasPlacedAStone       = not (null occupiedIntersections)
        occupiedIntersections = getOccupiedIntersections color board



getOccupiedIntersections :: Color -> Board -> [Intersection]
getOccupiedIntersections = (. join) . filter . flip hasColor



getTerritory :: Board -> Color -> [Intersection]
getTerritory board color = foldr (addTerritory board color) [] (concat board)



addTerritory :: Board -> Color -> Intersection -> [Intersection] -> [Intersection]
addTerritory board color intersection territory
  | isPrisoner                        = territory
  | Intersection.isEmpty intersection = territory ++ createTerritory board color intersection
  | otherwise                         = territory
  where isPrisoner                    = includes territory intersection



createTerritory :: Board -> Color -> Intersection -> [Intersection]
createTerritory board color intersection = fromMaybe [] territory
  where territory = addAdjacentsToTerritory board color [intersection]



addAdjacentsToTerritory :: Board -> Color -> [Intersection] -> Maybe [Intersection]
addAdjacentsToTerritory board color territory =
  foldr (addAdjacentToTerritory board color) (Just territory) adjacents
  where adjacents = getAdjacents board (head territory)



addAdjacentToTerritory :: Board -> Color -> Maybe Intersection -> Maybe [Intersection] -> Maybe [Intersection]
addAdjacentToTerritory board color maybeAdjacent maybeTerritory
  | isNothing maybeAdjacent           = maybeTerritory
  | isNothing maybeTerritory          = Nothing
  | isPrisoner                        = maybeTerritory
  | hasSameColor                      = maybeTerritory
  | Intersection.isEmpty intersection = addAdjacentsToTerritory board color (intersection:territory)
  | otherwise                         = Nothing
  where isPrisoner   = includes territory intersection
        hasSameColor = hasColor intersection color
        intersection = fromJust maybeAdjacent
        territory    = fromJust maybeTerritory



getAdjacents :: Board -> Intersection -> [Maybe Intersection]
getAdjacents board intersection = [top, right, bottom, left]
  where top    = getTop intersection board
        right  = getRight intersection board
        bottom = getBottom intersection board
        left   = getLeft intersection board



getTop :: Intersection -> Board -> Maybe Intersection
getTop = liftM2 getIntersection (subtract 1 . getX) getY



getRight :: Intersection -> Board -> Maybe Intersection
getRight = liftM2 getIntersection getX ((+) 1 . getY)



getBottom :: Intersection -> Board -> Maybe Intersection
getBottom = liftM2 getIntersection ((+) 1 .  getX) getY



getLeft :: Intersection -> Board -> Maybe Intersection
getLeft = liftM2 getIntersection getX (subtract 1 . getY)



getIntersection :: Int -> Int -> Board -> Maybe Intersection
getIntersection x y board = board ^? element y . element x

