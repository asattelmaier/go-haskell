{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.REST.Controller where

-------------------------------------------------------------------------------
import           Control.Lens
import qualified Data.ByteString.Char8 as B
import           Snap.Core
import           Snap.Snaplet



data Controller = Controller



makeLenses ''Controller



controller :: SnapletInit b Controller
controller =
  makeSnaplet "controller" "Go Haskell REST API Controller" Nothing $ do
  addRoutes routes
  return Controller



respondOk :: Handler b Controller ()
respondOk = do
  modifyResponse . setResponseCode $ 200



routes :: [(B.ByteString, Handler b Controller ())]
routes = [("status", method GET respondOk)]

