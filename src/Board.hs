module Board
( Board
, State (Empty, Stone)
, Color (Black, White)
, createBoard
, updatePosition
) where



data Color        = Black | White deriving (Show)
data State        = Empty | Stone Color deriving (Show)
type Intersection = State
type Board        = [[Intersection]]



createBoard :: Int -> Int -> Board
createBoard 0    cols = []
createBoard rows cols = createBoard (rows - 1) cols ++ [createIntersections cols]



createIntersections :: Int -> [Intersection]
createIntersections 0    = []
createIntersections cols = createIntersections (cols - 1) ++ [Empty]



updatePosition :: Board -> Int -> Int -> State -> Board
updatePosition (x:xs) horizontalLine verticalLine state
  | horizontalLine == 0  = updateState x verticalLine state:xs
  | otherwise            = x:updatePosition xs (horizontalLine - 1) verticalLine state



updateState :: [State] -> Int -> State -> [State]
updateState (x:xs) verticalLine state
  | verticalLine == 0  = state:xs
  | otherwise          = x:updateState xs (verticalLine - 1) state
