{-# LANGUAGE NamedFieldPuns #-}

module Go.Game
( Game (Game, positions, activePlayer, passivePlayer)
, Player
, Score
, createGame
, play
, pass
, end
) where



import Go.Board


type Score  = Int
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


-- TODO: Implement End Game
end :: Game -> Score
end game = 100



pass :: Game -> Maybe Game
pass Game {positions, activePlayer, passivePlayer}
  | isConsecutivePass positions = Nothing
  | otherwise                   = Just $ alternate Game { positions = head positions:positions
                                                        , activePlayer
                                                        , passivePlayer
                                                        }



isConsecutivePass :: [Board] -> Bool
isConsecutivePass board
  | length board < 2 = False
  | otherwise        = board!!0 == board!!1



play :: Game -> Location -> Game
play game location =
  maybe game alternate .
  prohibitRepetition .
  selfCapture .
  capture $
  playStone game location



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

