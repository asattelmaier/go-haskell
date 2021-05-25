{-# LANGUAGE RecordWildCards   #-}



module API.JSON.Client
( main
) where



import           API.JSON.Input.Command     (Command (NewGame, Pass, PlayStone))
import qualified API.JSON.Input.DTO         as Input (DTO (DTO), command, game,
                                                      location, size)
import qualified API.JSON.Output.DTO        as Output (DTO (DTO))
import           Data.Aeson                 (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL (pack, putStrLn)
import           Data.Maybe                 (fromJust)
import           Go.Game                    (EndGame (EndGame), create, end,
                                             pass, play)
import           Go.Settings                (Settings (Settings))



main :: String -> IO ()
main jsonString = do
  let inputDTO = decode (BL.pack jsonString) :: Maybe Input.DTO

  maybe (return ()) handleInputDTO inputDTO



handleInputDTO :: Input.DTO -> IO ()
handleInputDTO Input.DTO {..} =


  -- TODO: This logic can be reused.
  case command of
    NewGame   -> response $ Output.DTO newGame
      where newGame = create (Settings size (Just False))


    Pass      -> maybe (responseScore endGame) responseGame passGame
      where responseGame updatedGame = response $ Output.DTO updatedGame
            passGame     = pass (fromJust game)
            endGame      = end (fromJust game)


    PlayStone -> response $ Output.DTO playGame
      where playGame = play (fromJust game) (fromJust location)



response :: Output.DTO -> IO ()
response outputDTO = BL.putStrLn $ encode outputDTO



-- TODO: Add implementation for response Score
responseScore :: EndGame -> IO ()
responseScore (EndGame winner score) = BL.putStrLn (encode score)

