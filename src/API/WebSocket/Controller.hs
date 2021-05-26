module API.WebSocket.Controller
( handle
) where



import           API.WebSocket.Input.CreateDTO (CreateDTO)
import           API.WebSocket.Input.Data      (Data (CreateDTO, PassDTO, PlayDTO))
import           API.WebSocket.Input.PassDTO   (PassDTO)
import           API.WebSocket.Input.PlayDTO   (PlayDTO)
import qualified API.WebSocket.Service         as Service (create, pass, play)
import qualified Data.Aeson                    as JSON (ToJSON, decode, encode)
import           Data.ByteString.Lazy.Internal (ByteString)
import           Data.Text                     (pack)



handle :: ByteString -> ByteString
handle payload = do

  case JSON.decode payload of

    Just (CreateDTO dto) -> create dto

    Just (PlayDTO dto)   -> play dto

    Just (PassDTO dto)   -> pass dto

    Nothing              -> respondError "No valid data provided"



create :: CreateDTO -> ByteString
create = respond . Service.create



play :: PlayDTO -> ByteString
play = respond . Service.play



pass :: PassDTO -> ByteString
pass = respond . Service.pass



respond :: JSON.ToJSON a => a -> ByteString
respond = JSON.encode



respondError :: String -> ByteString
respondError = respond . pack . (++) "Error: "

