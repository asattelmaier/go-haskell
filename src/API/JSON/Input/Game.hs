{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Input.Game
( Game (Game)
) where



import           API.JSON.Input.Intersection ()
import           API.JSON.Input.Settings     ()
import           Data.Aeson                  (FromJSON, Value (Object),
                                              parseJSON, (.:))
import           Go.Game                     (Game (Game))



instance FromJSON Game where
  parseJSON (Object v) = Game
    <$> v .: "positions"
    <*> v .: "activePlayer"
    <*> v .: "passivePlayer"
    <*> v .: "settings"

