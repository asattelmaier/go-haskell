{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module API.JSON (main) where


import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Go.Board



data Game = Game
            { activePlayer :: Color
            , passivePlayer :: Color
            } deriving (Show)



instance ToJSON Color where
  toJSON Black = String "Black"
  toJSON White = String "White"



instance FromJSON Color where
  parseJSON color = case color of
    "Black" -> pure Black
    "White" -> pure White
    _       -> fail $ "Unknwon Color: " <> show color



instance ToJSON Game where
  toJSON (Game activePlayer passivePlayer) = object [ "activePlayer"  .= activePlayer,
                                                      "passivePlayer" .= passivePlayer ]



  toEncoding Game{..} = pairs $
    "activePlayer"  .= activePlayer <>
    "passivePlayer" .= passivePlayer



instance FromJSON Game where
  parseJSON (Object v) = Game <$>
    v .: "activePlayer" <*>
    v .: "passivePlayer"



main :: IO ()
main = do
  -- TODO: Just a test, add implementation
  print $ (decode "{\"activePlayer\": \"Black\",\"passivePlayer\":\"White\"}" :: Maybe Game)
  
  BL.putStrLn (encode (Game Black White))

