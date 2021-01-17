module UserInterface.Command
( Command (ExitGame, MoveCursor, PlaceStone)
, createCommand
) where



import UserInterface.Cursor (Cursor (Cursor))



data Command = ExitGame | MoveCursor Cursor | PlaceStone



createCommand :: Char -> Command
createCommand '\ESC' = ExitGame
createCommand 'k'    = MoveCursor (Cursor 0 (-1))
createCommand 'l'    = MoveCursor (Cursor 1 0)
createCommand 'j'    = MoveCursor (Cursor 0 1)
createCommand 'h'    = MoveCursor (Cursor (-1) 0)
createCommand 'f'    = PlaceStone
createCommand other  = MoveCursor (Cursor 0 0)

