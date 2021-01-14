module Render
( render
) where

import Position (Position (Position))
import Cursor (Cursor (Cursor))
import Player (Player (Player, cursor))
import Board (Board)

cursorRepresentation = '█'
upperRow = "+---"
lowerRow = "|   "

render :: Board -> Player -> String
render board player = renderPlayer renderedBoard player board
  where renderedBoard = renderBoard board

renderPlayer :: String -> Player -> Board -> String
renderPlayer renderedBoard (Player {cursor}) board =
  replaceChar renderedBoard index cursorRepresentation
  where index = getCursorPositionIndex cursor board

getCursorPositionIndex :: Cursor -> Board -> Int
getCursorPositionIndex (Cursor (Position x y)) board = x + y * (rows - 1) * 4 + y * 2
  where rows = length $ head board


-- TODO: Separate Functions in Modules


-- Create Board

renderBoard :: Board -> String
renderBoard board =
  foldr (++) (createUpperRow cols) $
    replicate rows $
      (createUpperRow cols) ++ (createLowerRow cols)
  where rows = length $ head board
        cols = length board

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
