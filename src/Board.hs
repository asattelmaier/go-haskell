module Board
( Board
, Point (Empty, Stone)
, Color (Black, White)
, createBoard
, setPoint
) where



data Color  = Black | White deriving (Show)
data Point = Empty | Stone Color deriving (Show)
type Board = [[Point]]



createBoard :: Int -> Int -> Board
createBoard rows cols = createPoints rows cols



createPoints :: Int -> Int -> [[Point]]
createPoints 0    cols = []
createPoints rows cols = (createPoints (rows - 1) cols) ++ [createRow cols]



createRow :: Int -> [Point]
createRow 0    = []
createRow cols = (createRow (cols - 1)) ++ [Empty]



setPoint :: Board -> Int -> Int -> Point -> Board
setPoint (x:xs) row col stone
  | row == 0  = (setPointInRow x col stone):xs
  | otherwise = x:setPoint xs (row - 1) col stone



setPointInRow :: [Point] -> Int -> Point -> [Point]
setPointInRow (x:xs) col point
  | col == 0  = point:xs
  | otherwise = x:setPointInRow xs (col - 1) point
