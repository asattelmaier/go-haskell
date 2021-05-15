{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}



module API.WebSocket.Input.CreateDTO
( DTO (DTO)
, getSize
, fromData
) where



import           API.WebSocket.Input.Data (Data, getCommandSize)
import           Control.Lens
import           Data.Maybe               (fromMaybe)



newtype DTO = DTO { _size :: Int
                  } deriving (Show)



makeLenses ''DTO



defaultGridSize :: Int
defaultGridSize = 19



fromData :: Data -> Maybe DTO
fromData = Just . DTO . fromMaybe defaultGridSize . getCommandSize



getSize :: DTO -> Int
getSize = view size

