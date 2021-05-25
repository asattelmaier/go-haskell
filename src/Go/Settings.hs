{-# LANGUAGE RecordWildCards #-}



module Go.Settings
( Settings (Settings)
, empty
, getIsSuicideAllowed
, getBoardSize
) where



import           Data.Maybe (fromMaybe)



data Settings = Settings { boardSize        :: Maybe Int
                         , isSuicideAllowed :: Maybe Bool
                         } deriving (Show)



empty :: Settings
empty = Settings { boardSize        = Nothing
                 , isSuicideAllowed = Nothing
                 }



getBoardSize :: Settings -> Int
getBoardSize Settings {..} = fromMaybe 19 boardSize



getIsSuicideAllowed :: Settings -> Bool
getIsSuicideAllowed Settings {..} = fromMaybe True isSuicideAllowed

