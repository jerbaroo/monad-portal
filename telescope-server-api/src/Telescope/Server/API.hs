{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Telescope.Server.API where

import           Servant.API           ( (:<|>), (:>), Capture, DeleteNoContent,
                                         Get, JSON, PostNoContent, ReqBody )
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
    :> PostNoContent
  :<|>
  "rmTable"
    :> Capture "tableKey" String
    :> DeleteNoContent

type WebSocketAPI = "watch" :> WebSocket
type API = RestAPI :<|> WebSocketAPI
