{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Input.Command
( Command (NewGame, PlayStone, Pass)
) where



import           Data.Aeson



data Command = NewGame
             | PlayStone
             | Pass
             deriving (Show)



instance ToJSON Command where
  toJSON NewGame   = String "NewGame"
  toJSON PlayStone = String "PlayStone"
  toJSON Pass      = String "Pass"



instance FromJSON Command where
  parseJSON command = case command of
    "NewGame"   -> pure NewGame
    "PlayStone" -> pure PlayStone
    "Pass"      -> pure Pass
    -- TODO: Adjust Error Handling
    _           -> fail $ "Unknwon Command: " <> show command
