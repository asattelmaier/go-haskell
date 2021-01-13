module Player
( Player (Player, cursor, color)
, Color (Black, White)
, movePlayer
) where

import Cursor (Cursor (Cursor), updateCursor)
import Position (Position (Position))


data Color = Black | White
data Player = Player { cursor :: Cursor
                     , color :: Color
                     }



movePlayer :: Player -> Position -> Player
movePlayer Player {cursor, color} position = Player {cursor = updateCursor cursor position, color}
