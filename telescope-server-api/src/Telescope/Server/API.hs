{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Telescope.Server.API where

import           Servant.API           ( (:<|>), (:>), Capture, DeleteNoContent,
                                         Get, JSON, NoContent, PostNoContent,
                                         Raw, ReqBody )
import           Servant.API.WebSocket ( WebSocket )

-- | A list of tuples of: RowKey and Row.
type TableAsList = [(String, String)]

type RestAPI =
  "viewTable"
    :> Capture "tableKey" String
    :> Get '[JSON] TableAsList
  :<|>
  "setTable"
    :> Capture "tableKey" String
    :> ReqBody '[JSON] TableAsList
    :> PostNoContent '[JSON] NoContent
  :<|>
  "rmTable"
    :> Capture "tableKey" String
    :> DeleteNoContent '[JSON] NoContent

type WebSocketAPI = "watch" :> WebSocket
type StaticAPI    = Raw
type API          = RestAPI :<|> WebSocketAPI :<|> StaticAPI
