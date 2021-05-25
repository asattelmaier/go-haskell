{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



module API.WebSocket.Input.PassCommandDTO
( PassCommandDTO (PassCommandDTO)
) where



import           API.WebSocket.Input.CommandDTO (CommandDTO (Pass))
import           Control.Applicative            (empty)
import           Data.Aeson                     (FromJSON, Value (Object),
                                                 parseJSON, (.:))



newtype PassCommandDTO = PassCommandDTO { name :: CommandDTO
                                        } deriving (Show)



instance FromJSON PassCommandDTO where
  parseJSON (Object v) = do
    name <- v .:  "name"

    case name of
      Pass -> return PassCommandDTO {..}
      _    -> empty

