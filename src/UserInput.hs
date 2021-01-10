module UserInput
( handleUserInput
) where

import Cursor(Cursor(Position))

handleUserInput :: Char -> Cursor
handleUserInput 'h' = Position (-1) 0
handleUserInput 'j' = Position 0 1
handleUserInput 'k' = Position 0 (-1)
handleUserInput 'l' = Position 1 0
handleUserInput  _  = Position 0 0
