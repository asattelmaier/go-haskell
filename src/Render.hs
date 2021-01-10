module Render
( render
, Grid(Grid)
) where

import Cursor(Cursor(Position))

data Board = Board String
data Grid = Grid Int Int

render :: Grid -> Cursor -> String
render grid cursor =
  boardToString $ setCursor board cursor grid
  where board = createBoard grid

setCursor :: Board -> Cursor -> Grid -> Board
setCursor board cursor grid =
  Board $ replaceChar (boardToString board) index '█'
  where index = getCursorPositionIndex cursor grid

getCursorPositionIndex :: Cursor -> Grid -> Int
getCursorPositionIndex (Position x y) (Grid rows _) = x + y * (rows - 1) * 4 + y * 2


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
