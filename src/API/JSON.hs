{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module API.JSON (main) where


import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy.Char8 as BL



data Game = Game { activePlayer :: Text, passivePlayer :: Text }
            deriving (Show)
             


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
  
  BL.putStrLn (encode (Game "Black" "White"))

