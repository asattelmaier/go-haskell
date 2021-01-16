{-# LANGUAGE NamedFieldPuns #-}

module Game
( Game (Game, board, activePlayer, passivePlayer)
, createGame
, placeStone
) where



import Board    (Board, State (Stone), Color (Black, White), createBoard, updatePosition)
import Location (Location (Location))



type Player = Color
data Game = Game { board         :: Board
                 , activePlayer  :: Player
                 , passivePlayer :: Player
                 }



createGame :: Int -> Game
createGame lines = Game { board         = createBoard lines lines
                        , activePlayer  = Black
                        , passivePlayer = White
                        }



placeStone :: Game -> Location -> Game
placeStone Game {board, activePlayer, passivePlayer} (Location x y) =
  Game { board         = updatePosition board x y (Stone activePlayer)
       , activePlayer  = passivePlayer
       , passivePlayer = activePlayer
       }

