{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}



module API.WebSocket.Input.PassDTO
( DTO (DTO)
, fromData
, getGame
) where



import           API.JSON.Input.Game
import qualified API.WebSocket.Input.Data as Data
import           Control.Lens



newtype DTO = DTO { _game :: Game
                  } deriving (Show)



makeLenses ''DTO



fromData :: Data.Data -> Maybe DTO
fromData = fmap DTO . Data.getGame



getGame :: DTO -> Game
getGame = view game

