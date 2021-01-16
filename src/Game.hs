{-# LANGUAGE NamedFieldPuns #-}

module Game
( Game (Game, board, activePlayer, passivePlayer)
, createGame
, placeStone
) where



import Board    (Board, Point (Stone), Color (Black, White), createBoard, setPoint)
import Position (Position (Position))



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



placeStone :: Game -> Position -> Game
placeStone (Game {board, activePlayer, passivePlayer}) (Position x y) =
  Game { board         = setPoint board x y (Stone activePlayer)
       , activePlayer  = passivePlayer
       , passivePlayer = activePlayer
       }

