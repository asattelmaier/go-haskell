module Go.Positions
( Positions
, Position
, get
, getLastPositions
, add
) where



import           Go.Board (Board)


type Position  = Board
type Positions = [Position]



get :: Int -> Positions -> Position
get index positions
  | length positions > index = positions!!index
  | otherwise                = []



getLastPositions :: Positions -> Positions
getLastPositions (position:positions) = positions
getLastPositions []                   = []



add :: Position -> Positions -> Positions
add = (:)

