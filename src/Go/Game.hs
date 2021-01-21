{-# LANGUAGE NamedFieldPuns #-}

module Go.Game
( Game (Game, positions, activePlayer, passivePlayer)
, createGame
, play
, pass
) where



import Go.Board



type Player = Color
data Game   = Game { positions     :: [Board]
                   , activePlayer  :: Player
                   , passivePlayer :: Player
                   }



createGame :: Int -> Game
createGame lines = Game { positions     = [createBoard lines]
                        , activePlayer  = Black
                        , passivePlayer = White
                        }



play :: Game -> Location -> Game
play game location =
  maybe game alternate .
  prohibitRepetition .
  selfCapture .
  capture $
  playStone game location



pass :: Game -> Game
pass Game {positions, activePlayer, passivePlayer} =
  alternate Game {positions = head positions:positions, activePlayer, passivePlayer}



playStone :: Game -> Location -> Game
playStone Game {positions = lastPreviousPosition:previousPositions, activePlayer, passivePlayer} location
  | isEmpty   = Game { positions = currentPosition:lastPreviousPosition:previousPositions
                     , activePlayer
                     , passivePlayer
                     }
  | otherwise = Game { positions = lastPreviousPosition:previousPositions, activePlayer, passivePlayer }
  where isEmpty         = isLocationEmpty lastPreviousPosition location
        currentPosition = placeStone lastPreviousPosition location (Stone activePlayer)



capture :: Game -> Game
capture Game {positions, activePlayer, passivePlayer} =
  updatePosition Game {positions, activePlayer, passivePlayer} passivePlayer



selfCapture :: Game -> Game
selfCapture Game {positions, activePlayer, passivePlayer} =
  updatePosition Game {positions, activePlayer, passivePlayer} activePlayer



updatePosition :: Game -> Player -> Game
updatePosition Game {positions = currentPosition:previousPositions, activePlayer, passivePlayer} player =
  Game { positions = updatedPosition:previousPositions
       , activePlayer
       , passivePlayer
       }
  where updatedPosition = removeStonesWithoutLiberty currentPosition player



prohibitRepetition :: Game -> Maybe Game
prohibitRepetition game
  | isRepeatingPosition game = Nothing
  | otherwise                = Just game



isRepeatingPosition :: Game -> Bool
isRepeatingPosition Game {positions = currentPosition:previousPositions}
  | length previousPositions < 2 = False
  | otherwise                    = currentPosition == previousPositions!!1



alternate :: Game -> Game
alternate Game {positions, activePlayer, passivePlayer} =
  Game { positions
       , activePlayer = passivePlayer
       , passivePlayer = activePlayer
       }

