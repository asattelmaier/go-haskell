{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Output.Game
( Game (Game)
) where



import           API.JSON.Output.Intersection ()
import           API.JSON.Output.Settings     ()
import           Data.Aeson                   (ToJSON, object, toJSON, (.=))
import           Go.Game                      (Game (Game))



instance ToJSON Game where
  toJSON (Game positions activePlayer passivePlayer settings) =
    object [ "positions"     .= positions,
             "activePlayer"  .= activePlayer,
             "passivePlayer" .= passivePlayer,
             "settings"      .= settings ]

