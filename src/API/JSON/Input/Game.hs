{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Input.Game 
( Game
) where



import Data.Aeson
import Go.Game
import API.JSON.Input.Intersection()



instance FromJSON Game where
  parseJSON (Object v) = Game
    <$> v .: "positions"
    <*> v .: "activePlayer"
    <*> v .: "passivePlayer"

