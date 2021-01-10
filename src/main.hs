data Board = Board String
data Grid = Grid Int Int
data Position = Position Int Int

getBoard :: Grid -> Position -> Board
getBoard grid position =
  addPosition board position grid
  where board = createBoard grid

addPosition :: Board -> Position -> Grid -> Board
addPosition board position grid =
  Board $ replaceChar (boardToString board) index '█'
  where index = getPositionIndex position grid

getPositionIndex :: Position -> Grid -> Int
getPositionIndex (Position x y) (Grid rows _) = x + y * (rows - 1) * 4 + (y * 2)


-- TODO: Separate Functions in Modules

-- Board Utils

boardToString :: Board -> String
boardToString (Board board) = board


-- Create Board

createBoard :: Grid -> Board
createBoard (Grid rows cols) =
  Board $ foldr (++) (createUpperRow cols) $
    replicate rows $
      (createUpperRow cols) ++ (createLowerRow cols)

createUpperRow :: Int -> String
createUpperRow cols = createRow cols "+---"

createLowerRow :: Int -> String
createLowerRow cols = createRow cols "|   "

createRow :: Int -> String -> String
createRow cols row = foldr (++) [head row, '\n'] $ replicate (cols - 1) row


-- General Utils

replaceChar :: String -> Int -> Char -> String
replaceChar [] _ _ = []
replaceChar (_:stringTail) 0 char = char:stringTail
replaceChar (stringHead:stringTail) index char =
  if index < 0
    then stringHead:stringTail
    else stringHead:replaceChar stringTail (index - 1) char
