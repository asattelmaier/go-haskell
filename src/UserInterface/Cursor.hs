module UserInterface.Cursor
( Cursor (Cursor)
, translateCursor
, createCursor
) where



data Cursor = Cursor Int Int



createCursor :: Cursor
createCursor = Cursor 0 0



translateCursor :: Cursor -> Cursor -> Cursor
translateCursor (Cursor x1 y1) (Cursor x2 y2) = Cursor (x1 + x2) (y1 + y2)
