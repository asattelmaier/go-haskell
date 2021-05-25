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
import           Go.Board      (Location, createBoard, getArea, placeStone,
                                removeStonesWithoutLiberty)
import qualified Go.Board      as Board (isEmpty)
import           Go.Player     (Player, createBlackPlayer, createWhitePlayer)
import           Go.Positions  (Position, Positions)
import qualified Go.Positions  as Positions (add, get, getLastPositions)
import           Go.Settings   (Settings)
import qualified Go.Settings   as Settings (getBoardSize, getIsSuicideAllowed)



type Score   = Int

data EndGame = EndGame { winner :: [Player]
                       , score  :: Score
                       } deriving (Show)

data Game    = Game    { positions     :: Positions
                       , activePlayer  :: Player
                       , passivePlayer :: Player
                       , settings      :: Settings
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
  <*> getSettings
    $ game



removePreviousPosition :: Game -> Game
removePreviousPosition = Game
    . getLastPositions
  <*> getActivePlayer
  <*> getPassivePlayer
  <*> getSettings



getActivePlayer :: Game -> Player
getActivePlayer = activePlayer



getPassivePlayer :: Game -> Player
getPassivePlayer = passivePlayer



getSettings :: Game -> Settings
getSettings = settings



getIsSuicideAllowed :: Game -> Bool
getIsSuicideAllowed = Settings.getIsSuicideAllowed . getSettings



getPlayers :: Game -> [Player]
getPlayers game = [getActivePlayer game, getPassivePlayer game]



create :: Settings -> Game
create settings = Game
  { positions     = [createBoard $ Settings.getBoardSize settings]
  , activePlayer  = createBlackPlayer
  , passivePlayer = createWhitePlayer
  , settings      = settings
  }



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
  | isEmpty game location = maybe
                              game
                              alternate .
                            prohibitRepetition .
                            selfCapture .
                            capture $
                            playStone game location
  | otherwise             = game



isEmpty :: Game -> Location -> Bool
isEmpty = Board.isEmpty . getPosition 0



playStone :: Game -> Location -> Game
playStone = liftM2 (.) addPosition createPosition



createPosition :: Game -> Location -> Position
createPosition = placeStone . getPosition 0 <*> getActivePlayer



capture :: Game -> Game
capture = updatePosition <*> getPassivePlayer



selfCapture :: Game -> Maybe Game
selfCapture game
  | not isSuicideAllowed && isSuicide = Nothing
  | otherwise                         = Just capturedGame
  where isSuicideAllowed = getIsSuicideAllowed game
        isSuicide        = getPosition 0 game /= getPosition 0 capturedGame
        capturedGame     = updatePosition <*> getActivePlayer $ game



updatePosition :: Game -> Player -> Game
updatePosition = liftM2 (.)
  (addPosition . removePreviousPosition)
  (removeStonesWithoutLiberty . getPosition 0)



prohibitRepetition :: Maybe Game -> Maybe Game
prohibitRepetition Nothing     = Nothing
prohibitRepetition (Just game)
  | isRepeatingPosition game   = Nothing
  | otherwise                  = Just game



isRepeatingPosition :: Game -> Bool
isRepeatingPosition = (==) . getPosition 0 <*> getPosition 2



copyLatestPosition :: Game -> Game
copyLatestPosition = addPosition <*> getPosition 0



alternate :: Game -> Game
alternate = Game . getPositions
  <*> getPassivePlayer
  <*> getActivePlayer
  <*> getSettings

