module Command
( Command (ExitGame, MoveCursor, PlaceStone)
, createCommand
) where

import Position (Position (Position))


data Command = ExitGame | MoveCursor Position | PlaceStone

createCommand :: Char -> Command
createCommand '\ESC' = ExitGame
createCommand 'k'    = MoveCursor (Position 0 (-1))
createCommand 'l'    = MoveCursor (Position 1 0)
createCommand 'j'    = MoveCursor (Position 0 1)
createCommand 'h'    = MoveCursor (Position (-1) 0)
createCommand 'g'    = PlaceStone
createCommand other  = MoveCursor (Position 0 0)

