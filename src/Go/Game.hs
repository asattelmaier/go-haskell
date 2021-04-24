{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Go.Game
( Game (Game, positions, activePlayer, passivePlayer)
, Player
, Score
, EndGame (EndGame)
, createGame
, play
, pass
, end
) where



import           Go.Board



type Player  = Color

type Score   = Int

data EndGame = EndGame { winner :: [Player]
                       , score  :: Score
                       } deriving (Show)

data Game    = Game    { positions     :: [Board]
                       , activePlayer  :: Player
                       , passivePlayer :: Player
                       } deriving (Show)



createGame :: Int -> Game
createGame grid = Game { positions     = [createBoard grid]
                       , activePlayer  = Black
                       , passivePlayer = White
                       }



end :: Game -> EndGame
end Game {..}
  | activePlayerScore > passivePlayerScore  = EndGame [activePlayer] activePlayerScore
  | activePlayerScore == passivePlayerScore = EndGame [activePlayer, passivePlayer] activePlayerScore
  | otherwise                               = EndGame [passivePlayer] passivePlayerScore
  where activePlayerScore  = getScore (head positions) activePlayer
        passivePlayerScore = getScore (head positions) passivePlayer



getScore :: Board -> Player -> Int
getScore board = length . getArea board



pass :: Game -> Maybe Game
pass Game {..}
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
isEmpty Game {..} = isLocationEmpty (head positions)



playStone :: Game -> Location -> Game
playStone Game {positions = lastPreviousPosition:previousPositions, activePlayer, passivePlayer} location =
  Game {positions = currentPosition:lastPreviousPosition:previousPositions, activePlayer, passivePlayer}
  where currentPosition = placeStone lastPreviousPosition location (Stone activePlayer)



capture :: Game -> Game
capture Game {..} =
  updatePosition Game {positions, activePlayer, passivePlayer} passivePlayer



selfCapture :: Game -> Game
selfCapture Game {..} =
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
alternate Game {..} = Game positions passivePlayer activePlayer

