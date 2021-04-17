{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}



module API.WebSocket.Input.Data
( Data (Data)
, getCommand
, getGame
, getCommandName
, getCommandLocation
, getCommandSize
) where



import           API.JSON.Input.Game
import           API.JSON.Input.Location
import           API.WebSocket.Input.Command
import           Control.Lens
import           Data.Aeson



data Data = Data { _command :: Command
                 , _game    :: Maybe Game
                 } deriving (Show)



makeLenses ''Data



instance FromJSON Data where
  parseJSON (Object v) = Data
    <$> v .:  "command"
    <*> v .:? "game"



getCommand :: Data -> Command
getCommand = view command



getGame :: Data -> Maybe Game
getGame = view game



getCommandName :: Data -> Name
getCommandName = view name . getCommand



getCommandLocation :: Data -> Maybe Location
getCommandLocation = view location . getCommand



getCommandSize :: Data -> Maybe Int
getCommandSize = view size . getCommand

