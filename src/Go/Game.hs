{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}



module Go.Game
( Game (Game, positions, activePlayer, passivePlayer)
, Player
, Score
, EndGame (EndGame)
, create
, play
, pass
, end
) where



import           Control.Monad (liftM2)
import           Go.Board      (Location, createBoard, getArea, isLocationEmpty,
                                placeStone, removeStonesWithoutLiberty)
import           Go.Player     (Player, createBlackPlayer, createWhitePlayer)
import           Go.Positions  (Position, Positions)
import qualified Go.Positions  as Positions (add, get, getLastPositions)



type Score   = Int

data EndGame = EndGame { winner :: [Player]
                       , score  :: Score
                       } deriving (Show)

data Game    = Game    { positions     :: Positions
                       , activePlayer  :: Player
                       , passivePlayer :: Player
                       } deriving (Show)



getPositions :: Game -> Positions
getPositions Game {..} = positions



getPosition :: Int -> Game -> Position
getPosition = (. getPositions) . Positions.get



getLastPositions :: Game -> Positions
getLastPositions = Positions.getLastPositions . getPositions



addPosition :: Game -> Position -> Game
addPosition game position = Game
    . (Positions.add position . getPositions)
  <*> getActivePlayer
  <*> getPassivePlayer
    $ game



removePreviousPosition :: Game -> Game
removePreviousPosition = Game
    . getLastPositions
  <*> getActivePlayer
  <*> getPassivePlayer



getActivePlayer :: Game -> Player
getActivePlayer Game {..} = activePlayer



getPassivePlayer :: Game -> Player
getPassivePlayer Game {..} = passivePlayer



getPlayers :: Game -> [Player]
getPlayers game = [getActivePlayer game, getPassivePlayer game]


create :: Int -> Game
create size = Game [createBoard size] createBlackPlayer createWhitePlayer



end :: Game -> EndGame
end game
  | hasActivePlayerWon game = createEndGame game [getActivePlayer game]
  | isDrawn game            = createEndGame <*> getPlayers $ game
  | otherwise               = createEndGame game [getPassivePlayer game]



createEndGame :: Game -> [Player] -> EndGame
createEndGame game winner = EndGame winner (getScore game $ head winner)


isDrawn :: Game -> Bool
isDrawn = (==)
    . (getScore <*> getActivePlayer)
  <*> (getScore <*> getPassivePlayer)



hasActivePlayerWon :: Game -> Bool
hasActivePlayerWon = (>)
    . (getScore <*> getActivePlayer)
  <*> (getScore <*> getPassivePlayer)



getScore :: Game -> Player -> Int
getScore board = length . getArea (getPosition 0 board)



pass :: Game -> Maybe Game
pass game
  | isConsecutivePass game = Nothing
  | otherwise              = Just . alternate . copyLatestPosition $ game



isConsecutivePass :: Game -> Bool
isConsecutivePass = (==) . getPosition 0 <*> getPosition 1



play :: Game -> Location -> Game
play game location
  | isEmpty game location = maybe game alternate .
                            prohibitRepetition .
                            selfCapture .
                            capture $
                            playStone game location
  | otherwise             = game



isEmpty :: Game -> Location -> Bool
isEmpty = isLocationEmpty . getPosition 0



playStone :: Game -> Location -> Game
playStone = liftM2 (.) addPosition createPosition



createPosition :: Game -> Location -> Position
createPosition = placeStone . getPosition 0 <*> getActivePlayer



capture :: Game -> Game
capture = updatePosition <*> getPassivePlayer



selfCapture :: Game -> Game
selfCapture = updatePosition <*> getActivePlayer



updatePosition :: Game -> Player -> Game
updatePosition = liftM2 (.)
  (addPosition . removePreviousPosition)
  (removeStonesWithoutLiberty . getPosition 0)



prohibitRepetition :: Game -> Maybe Game
prohibitRepetition game
  | isRepeatingPosition game = Nothing
  | otherwise                = Just game



isRepeatingPosition :: Game -> Bool
isRepeatingPosition = (==) . getPosition 0 <*> getPosition 2



copyLatestPosition :: Game -> Game
copyLatestPosition = addPosition <*> getPosition 0



alternate :: Game -> Game
alternate = Game . getPositions <*> getPassivePlayer <*> getActivePlayer

