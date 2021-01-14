module Render
( render
) where



import Position (Position (Position))
import Cursor   (Cursor (Cursor))
import Player   (Player (Player, cursor))
import Board    (Board)
import Game     (Game (Game, board, activePlayer))



cursorRepresentation = '█'
upperRow = "+---"
lowerRow = "|   "



render :: Game -> String
render game = renderedGame
  where renderedGame  = renderPlayer renderedBoard game
        renderedBoard = renderBoard $ getBoard game
        getBoard Game {board} = board



renderPlayer :: String -> Game -> String
renderPlayer renderedBoard (Game {board, activePlayer = Player {cursor}}) =
  replaceChar renderedBoard index cursorRepresentation
  where index = getCursorPositionIndex cursor board



getCursorPositionIndex :: Cursor -> Board -> Int
getCursorPositionIndex (Cursor (Position x y)) board = x + y * (rows - 1) * 4 + y * 2
  where rows = length $ head board



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
