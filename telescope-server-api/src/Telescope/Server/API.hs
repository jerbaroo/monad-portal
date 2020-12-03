{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeOperators          #-}

module Telescope.Server.API where

import           Servant.API                ( (:<|>), (:>), JSON, NoContent,
                                              PostNoContent, Raw, ReqBody, Post )
import           Servant.API.WebSocket      ( WebSocket )
import           Telescope.Server.API.Types ( RowIndices, TableKey, Tables )

type RestAPI =
  "viewRows"
    :> ReqBody       '[JSON] RowIndices
    :> Post          '[JSON] Tables
  :<|>
  "viewTables"
    :> ReqBody       '[JSON] [TableKey]
    :> Post          '[JSON] Tables
  :<|>
  "setRows"
    :> ReqBody       '[JSON] Tables
    :> PostNoContent '[JSON] NoContent
  :<|>
  "setTables"
    :> ReqBody       '[JSON] Tables
    :> PostNoContent '[JSON] NoContent
  :<|>
  "rmRows"
    :> ReqBody       '[JSON] RowIndices
    :> PostNoContent '[JSON] NoContent
  :<|>
  "rmTables"
    :> ReqBody       '[JSON] [TableKey]
    :> PostNoContent '[JSON] NoContent

type WebSocketAPI = "watchRow" :> WebSocket :<|> "watchTable" :> WebSocket
type StaticAPI    = Raw
type API          = RestAPI :<|> WebSocketAPI :<|> StaticAPI
