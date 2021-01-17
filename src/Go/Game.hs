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
  capture $
  playStone game location



playStone :: Game -> Location -> Game
playStone Game {board, activePlayer, passivePlayer} location
  | isEmpty     = Game { board         = placeStone board location (Stone activePlayer)
                       , activePlayer  = passivePlayer
                       , passivePlayer = activePlayer
                       }
  | otherwise   = Game {board, activePlayer, passivePlayer}
  where isEmpty = isLocationEmpty board location



capture :: Game -> Game
capture Game {board, activePlayer, passivePlayer} =
  Game { board = removeStonesWithoutLiberties board
       , activePlayer
       , passivePlayer
       }
