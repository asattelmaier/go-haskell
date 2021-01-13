module Cursor
( Cursor (Cursor)
, updateCursor
) where

import Position (Position (Position))


data Cursor = Cursor Position

updateCursor :: Cursor -> Position -> Cursor
updateCursor (Cursor (Position x1 y1)) (Position x2 y2) = Cursor (Position (x1 + x2) (y1 + y2))
