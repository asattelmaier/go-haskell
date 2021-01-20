module UserInterface.Command
( Command (ExitGame, MoveCursor, PlayStone)
, createCommand
) where



import UserInterface.Cursor (Cursor (Cursor))



data Command = ExitGame | MoveCursor Cursor | PlayStone



createCommand :: Char -> Command
createCommand '\ESC' = ExitGame
createCommand 'k'    = MoveCursor (Cursor 0 (-1))
createCommand 'l'    = MoveCursor (Cursor 1 0)
createCommand 'j'    = MoveCursor (Cursor 0 1)
createCommand 'h'    = MoveCursor (Cursor (-1) 0)
createCommand 'f'    = PlayStone
createCommand other  = MoveCursor (Cursor 0 0)

