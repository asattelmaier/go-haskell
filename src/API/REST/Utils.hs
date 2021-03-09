{-# LANGUAGE OverloadedStrings #-}



module API.REST.Utils
( getRequestBody
, responseJSON
, handlePOST
) where



import           Control.Lens
import           Data.Aeson
import           Data.Maybe   (fromJust)
import           GHC.Word
import           Snap.Core



maxRequestBodySize :: Word64
maxRequestBodySize = 1000000



handlePOST :: (MonadSnap m, FromJSON a1, ToJSON a2) => (a1 -> a2) -> m ()
handlePOST service = getRequestBody >>= responseJSON . service


-- TODO: Add Error Handling
getRequestBody :: (MonadSnap m, FromJSON b) => m b
getRequestBody = readRequestBody maxRequestBodySize <&> (fromJust . decode)



responseJSON :: (MonadSnap m, ToJSON a) => a -> m ()
responseJSON jsonResponse = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS $ encode jsonResponse

