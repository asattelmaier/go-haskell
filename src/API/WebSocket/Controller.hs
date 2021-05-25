module API.WebSocket.Controller
( handle
) where



import           API.WebSocket.Input.CreateDTO (CreateDTO)
import           API.WebSocket.Input.Data      (Data (CreateDTO, PassDTO, PlayDTO))
import           API.WebSocket.Input.PassDTO   (PassDTO)
import           API.WebSocket.Input.PlayDTO   (PlayDTO)
import qualified API.WebSocket.Service         as Service (create, pass, play)
import qualified Data.Aeson                    as JSON (ToJSON, Value (String),
                                                        decode, encode)
import           Data.Text                     (pack)
import           Data.Text.Lazy                (Text)
import           Data.Text.Lazy.Encoding       (decodeUtf8, encodeUtf8)



handle :: Text -> Text
handle rawData = do

  case JSON.decode . encodeUtf8 $ rawData of

    Just (CreateDTO dto) -> create dto

    Just (PlayDTO dto)   -> play dto

    Just (PassDTO dto)   -> pass dto

    Nothing              -> respondError "No valid data provided"



create :: CreateDTO -> Text
create = respond . Service.create



play :: PlayDTO -> Text
play = respond . Service.play



pass :: PassDTO -> Text
pass = respond . Service.pass



respond :: JSON.ToJSON a => a -> Text
respond = decodeUtf8 . JSON.encode



respondError :: String -> Text
respondError = respond . JSON.String . pack . (++) "Error: "

