{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Output.Game
( Go.Game
) where



import           API.JSON.Output.Intersection ()
import           Data.Aeson
import qualified Go.Game                      as Go



instance ToJSON Go.Game where
  toJSON (Go.Game positions activePlayer passivePlayer) =
    object [ "positions"     .= positions,
             "activePlayer"  .= activePlayer,
             "passivePlayer" .= passivePlayer ]
