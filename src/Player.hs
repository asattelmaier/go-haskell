module Player
( Player (Player, cursor, color)
, Color (Black, White)
, createPlayer
, movePlayer
) where

import Cursor (Cursor (Cursor), updateCursor)
import Position (Position (Position))


data Color = Black | White deriving (Show)
data Player = Player { cursor :: Cursor
                     , color :: Color
                     }

createPlayer :: Color -> Player
createPlayer color = Player {cursor = Cursor (Position 0 0), color}

movePlayer :: Player -> Position -> Player
movePlayer Player {cursor, color} position = Player {cursor = updatedCursor, color}
  where updatedCursor = updateCursor cursor position 


