module Cursor
( Cursor
, translateCursor
, createCursor
) where



import Position (Position (Position))



type Cursor = Position



createCursor :: Cursor
createCursor = Position 0 0



translateCursor :: Cursor -> Cursor -> Cursor
translateCursor (Position x1 y1) (Position x2 y2) = Position (x1 + x2) (y1 + y2)
