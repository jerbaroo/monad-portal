{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Telescope.Server.API where

import qualified Data.Map               as Map
import           Servant.API            ( (:<|>), (:>), Capture,
                                          DeleteNoContent, Get, JSON, NoContent,
                                          PostNoContent, Raw, ReqBody )
import           Servant.API.WebSocket  ( WebSocket )
import qualified Telescope.Table        as Table'

type TableName = String
type Table     = (TableName, [(Table'.RowKey, Table'.Row)])
type Tables    = [Table]

-- | Convert Telescope Tables to API-format tables.
toAPITables :: Table'.Tables -> Tables
toAPITables = map f . Map.toList
  where f (Table'.TableKey tn, table) = (tn, Map.toList table)

-- | Convert API-format tables to Telescope Tables.
fromAPITables :: Tables -> Table'.Tables
fromAPITables = Map.fromList . map f
  where f (tn, table) = (Table'.TableKey tn, Map.fromList table)

type RestAPI =
  "viewTables"
    :> ReqBody         '[JSON] [TableName]
    :> Get             '[JSON] Tables
  :<|>
  "setTables"
    :> ReqBody         '[JSON] Tables
    :> PostNoContent   '[JSON] NoContent
  :<|>
  "rmTables"
    :> ReqBody         '[JSON] [TableName]
    :> DeleteNoContent '[JSON] NoContent

type WebSocketAPI = "watch" :> WebSocket
type StaticAPI    = Raw
type API          = RestAPI :<|> WebSocketAPI :<|> StaticAPI
