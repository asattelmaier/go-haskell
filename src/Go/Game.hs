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



end :: Game -> ([Player], Score)
end Game {positions, activePlayer, passivePlayer}
  | activePlayerScore > passivePlayerScore  = ([activePlayer], activePlayerScore)
  | activePlayerScore == passivePlayerScore = ([activePlayer, passivePlayer], activePlayerScore)
  | otherwise                               = ([passivePlayer], passivePlayerScore)
  where activePlayerScore  = getScore (head positions) activePlayer
        passivePlayerScore = getScore (head positions) passivePlayer



getScore :: Board -> Player -> Score
getScore position player = length area
  where area = getArea position player



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
  | otherwise        = head board == board!!1



play :: Game -> Location -> Game
play game location
  | isEmpty game location = maybe game alternate .
                            prohibitRepetition .
                            selfCapture .
                            capture $
                            playStone game location
  | otherwise             = game



isEmpty :: Game -> Location -> Bool
isEmpty Game {positions} = isLocationEmpty (head positions)



playStone :: Game -> Location -> Game
playStone Game {positions = lastPreviousPosition:previousPositions, activePlayer, passivePlayer} location =
  Game {positions = currentPosition:lastPreviousPosition:previousPositions, activePlayer, passivePlayer}
  where currentPosition = placeStone lastPreviousPosition location (Stone activePlayer)



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

