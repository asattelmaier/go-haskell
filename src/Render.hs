{-# LANGUAGE NamedFieldPuns #-}

module Render
( render
, cursorToPosition
) where



import Position (Position (Position))
import Board    (Board, Point (Empty, Stone), Color (White, Black))
import Game     (Game (Game, board, activePlayer))
import Cursor   (Cursor)



cursorRepresentation = '█'
emptyPoint           = "+---"
blackPoint           = "X---"
whitePoint           = "O---"
separatorRow         = "|   "



render :: Game -> Cursor -> String
render game cursor = renderCursor game cursor $ renderBoard game



cursorToPosition :: Cursor -> Maybe Position
cursorToPosition (Position x y)
  | isOnPoint = Just (Position positionY positionX)
  | otherwise = Nothing
  where positionX  = x `div` (length emptyPoint)
        positionY  = y `div` 2
        isOnPoint  = isXOnPoint && isYOnPoint
        isXOnPoint = x `mod` (length emptyPoint) == 0
        isYOnPoint = y `mod` 2 == 0



renderCursor :: Game -> Cursor -> String -> String
renderCursor (Game {board}) cursor renderedBoard =
  replaceChar renderedBoard index cursorRepresentation
  where index = getCursorPositionIndex cursor board



getCursorPositionIndex :: Cursor -> Board -> Int
getCursorPositionIndex (Position x y) board = x + y * (rows - 1) * 4 + y * 2
  where rows = length $ head board



renderBoard :: Game -> String
renderBoard Game {board} = removeLastLine $ concat $ map renderPoints board
  where removeLastLine = take $ rows * rowWidth * 2 - rowWidth
        rows           = length board
        rowWidth       = colWidth * (rows - 1) + 2
        colWidth       = length emptyPoint



renderPoints :: [Point] -> String
renderPoints points = unlines $ [upperRow] ++ [lowerRow]
  where upperRow = renderRow $ map renderPoint points
        lowerRow = renderRow $ replicate (length points) separatorRow



renderRow :: [String] -> String
renderRow a = take ((length a - 1) * (length emptyPoint) + 1) $ concat a



renderPoint :: Point -> String
renderPoint Empty         = emptyPoint
renderPoint (Stone Black) = blackPoint
renderPoint (Stone White) = whitePoint





-- General Utils

replaceChar :: String -> Int -> Char -> String
replaceChar [] _ _ = []
replaceChar (_:stringTail) 0 char = char:stringTail
replaceChar (stringHead:stringTail) index char =
  if index < 0
    then stringHead:stringTail
    else stringHead:replaceChar stringTail (index - 1) char
