module Command
( Command (Exit, Move)
, createCommand
, commandToPosition
) where

import Position (Position (Position))


data Command = Exit | Move Position deriving (Show, Eq)

createCommand :: Char -> Command
createCommand '\ESC' = Exit
createCommand 'k'    = Move (Position 0 (-1))
createCommand 'l'    = Move (Position 1 0)
createCommand 'j'    = Move (Position 0 1)
createCommand 'h'    = Move (Position (-1) 0)
createCommand other  = Move (Position 0 0)

commandToPosition :: Command -> Position
commandToPosition (Move (Position x y)) = Position x y
