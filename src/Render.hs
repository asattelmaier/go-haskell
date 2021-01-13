module Render
( render
, Grid(Grid)
) where

import Position (Position (Position))
import Cursor (Cursor (Cursor))
import Player (Player (Player, cursor))


data Board = Board String
data Grid = Grid Int Int

cursorRepresentation = '█'
upperRow = "+---"
lowerRow = "|   "

render :: Grid -> Player -> String
render grid player =
  boardToString $ setPlayer board player grid
  where board = createBoard grid

setPlayer :: Board -> Player -> Grid -> Board
setPlayer board (Player {cursor}) grid =
  Board $ replaceChar (boardToString board) index cursorRepresentation
  where index = getCursorPositionIndex cursor grid

getCursorPositionIndex :: Cursor -> Grid -> Int
getCursorPositionIndex (Cursor (Position x y)) (Grid rows _) = x + y * (rows - 1) * 4 + y * 2


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
createUpperRow cols = createRow cols upperRow

createLowerRow :: Int -> String
createLowerRow cols = createRow cols lowerRow

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
