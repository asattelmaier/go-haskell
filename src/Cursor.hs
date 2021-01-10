module Cursor
( Cursor(Position)
, updateCursor
) where

data Cursor = Position Int Int deriving (Show)

updateCursor :: Cursor -> Cursor -> Cursor
updateCursor (Position x1 y1) (Position x2 y2) = Position (x1 + x2) (y1 + y2)

