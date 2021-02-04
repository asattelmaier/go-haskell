{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module API.JSON (main) where


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
  parseJSON (Object v) = Location <$>
    v .: "x" <*>
    v .: "y"



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

data Command = PlayStone | Pass deriving (Show)



instance ToJSON Command where
  toJSON PlayStone = String "PlayStone"
  toJSON Pass      = String "Pass"



instance FromJSON Command where
  parseJSON command = case command of
    "PlayStone" -> pure PlayStone
    "Pass"      -> pure Pass
    _           -> fail $ "Unknwon Command: " <> show command



-------------------------------------------------------------------------------
-- Play
-------------------------------------------------------------------------------

data Play = Play { game :: Game
                 , command :: Command
                 } deriving (Show)



instance ToJSON Play where
  toJSON (Play game command) =
    object [ "game"    .= game,
             "command" .= command ]

  toEncoding Play{..} = pairs $
    "game"    .= game <>
    "command" .= command



instance FromJSON Play where
  parseJSON (Object v) = Play
    <$> v .: "game"
    <*> v .: "command"



main :: IO ()
main = do
  -- TODO: Just a test, add implementation
  print (decode "{\"game\":{\"positions\":[[[{\"location\": {\"x\": 2, \"y\": 1}, \"state\": \"Black\"}]]],\"activePlayer\": \"Black\",\"passivePlayer\":\"White\"},\"command\":\"Pass\"}" :: Maybe Play)
  
  BL.putStrLn (encode (Play (createGame 2) PlayStone))

