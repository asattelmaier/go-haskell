{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module API.JSON (main) where



import Data.Maybe
import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Go.Board
import Go.Game



-------------------------------------------------------------------------------
-- State
-------------------------------------------------------------------------------

instance ToJSON State where
  toJSON Empty         = String "Empty"
  toJSON (Stone color) = toJSON color



instance FromJSON State where
  parseJSON state = case state of
    "Empty" -> pure Empty
    "Black" -> pure (Stone Black)
    "White" -> pure (Stone White)



-------------------------------------------------------------------------------
-- Location
-------------------------------------------------------------------------------

instance ToJSON Location where
  toJSON (Location x y) =
    object [ "x" .= x,
             "y" .= y ]
  
  toEncoding (Location x y) = pairs $
    "x" .= x <>
    "y" .= y



instance FromJSON Location where
  parseJSON (Object v) = Location
    <$> v .: "x"
    <*> v .: "y"
  parseJSON _          = empty



-------------------------------------------------------------------------------
-- Intersection
-------------------------------------------------------------------------------

instance ToJSON Intersection where
  toJSON (Intersection location state) =
    object [ "location" .= location,
             "state"    .= state ]
  
  toEncoding (Intersection location state) = pairs $
    "location" .= location <>
    "state"    .= state



instance FromJSON Intersection where
  parseJSON (Object v) = Intersection
    <$> v .: "location"
    <*> v .: "state"



-------------------------------------------------------------------------------
-- Color
------------------------------------------------------------------------------  -

instance ToJSON Color where
  toJSON Black = String "Black"
  toJSON White = String "White"



instance FromJSON Color where
  parseJSON color = case color of
    "Black" -> pure Black
    "White" -> pure White
    _       -> fail $ "Unknwon Color: " <> show color



-------------------------------------------------------------------------------
-- Game
-------------------------------------------------------------------------------

instance ToJSON Game where
  toJSON (Game positions activePlayer passivePlayer) =
    object [ "positions"     .= positions,
             "activePlayer"  .= activePlayer,
             "passivePlayer" .= passivePlayer ]

  toEncoding Game{..} = pairs $
    "positions"     .= positions <>
    "activePlayer"  .= activePlayer <>
    "passivePlayer" .= passivePlayer



instance FromJSON Game where
  parseJSON (Object v) = Game
    <$> v .: "positions"
    <*> v .: "activePlayer"
    <*> v .: "passivePlayer"




-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

data Command = NewGame | PlayStone | Pass deriving (Show)



instance ToJSON Command where
  toJSON NewGame   = String "NewGame"
  toJSON PlayStone = String "PlayStone"
  toJSON Pass      = String "Pass"



instance FromJSON Command where
  parseJSON command = case command of
    "NewGame"   -> pure NewGame
    "PlayStone" -> pure PlayStone
    "Pass"      -> pure Pass
    _           -> fail $ "Unknwon Command: " <> show command



-------------------------------------------------------------------------------
-- PlayData
-------------------------------------------------------------------------------

data PlayData = PlayData { game :: Game
                         , command :: Command
                         , location :: Maybe Location
                         } deriving (Show)



instance ToJSON PlayData where
  toJSON (PlayData game command location) =
    object [ "game"     .= game,
             "command"  .= command,
             "location" .= location ]

  toEncoding PlayData{..} = pairs $
    "game"     .= game <>
    "command"  .= command <>
    "location" .= location



instance FromJSON PlayData where
  parseJSON (Object v) = PlayData
    <$> v .: "game"
    <*> v .: "command"
    <*> v .:? "location"
  



main :: String -> IO ()
main jsonData = do
  let playData = decode (BL.pack jsonData) :: Maybe PlayData

  maybe (response playData) handlePlayData playData



handlePlayData :: PlayData -> IO ()
handlePlayData PlayData {..} =

  
  case command of
    NewGame   -> response $ Just $ PlayData (createGame 19) command location

   
    Pass      -> maybe (responseScore endGame) responseGame passGame
      where responseGame = \updatedGame -> response $ Just $ PlayData updatedGame command location
            passGame     = pass game
            endGame      = end game
    

    PlayStone -> response $ Just $ PlayData (play game (fromJust location)) command location



response :: Maybe PlayData -> IO ()
response playData = BL.putStrLn (maybe BL.empty encode playData)



-- TODO: Add implementation for response Score
responseScore :: ([Color], Score) -> IO ()
responseScore score = BL.putStrLn (encode score)