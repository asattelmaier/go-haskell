{-# LANGUAGE NamedFieldPuns #-}

module Go.Game
( Game (Game, board, activePlayer, passivePlayer)
, createGame
, play
) where



import Go.Board



type Player = Color
data Game = Game { board         :: Board
                 , activePlayer  :: Player
                 , passivePlayer :: Player
                 }



createGame :: Int -> Game
createGame lines = Game { board         = createBoard lines
                        , activePlayer  = Black
                        , passivePlayer = White
                        }



play :: Game -> Location -> Game
play game location =
  alternate .
  selfCapture .
  capture $
  playStone game location



playStone :: Game -> Location -> Game
playStone Game {board, activePlayer, passivePlayer} location
  | isEmpty     = Game { board         = placeStone board location (Stone activePlayer)
                       , activePlayer
                       , passivePlayer
                       }
  | otherwise   = Game {board, activePlayer, passivePlayer}
  where isEmpty = isLocationEmpty board location



capture :: Game -> Game
capture Game {board, activePlayer, passivePlayer} =
  Game { board = removeStonesWithoutLiberty board passivePlayer
       , activePlayer
       , passivePlayer
       }



selfCapture :: Game -> Game
selfCapture Game {board, activePlayer, passivePlayer} =
  Game { board = removeStonesWithoutLiberty board activePlayer
       , activePlayer
       , passivePlayer
       }



alternate :: Game -> Game
alternate Game {board, activePlayer, passivePlayer} =
  Game { board
       , activePlayer = passivePlayer
       , passivePlayer = activePlayer
       }
