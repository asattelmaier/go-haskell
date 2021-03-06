{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Input.Game
( Game
) where



import           API.JSON.Input.Intersection ()
import           Data.Aeson
import           Go.Game



instance FromJSON Game where
  parseJSON (Object v) = Game
    <$> v .: "positions"
    <*> v .: "activePlayer"
    <*> v .: "passivePlayer"

