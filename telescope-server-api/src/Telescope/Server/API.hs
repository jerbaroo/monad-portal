{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Telescope.Server.API where

import qualified Data.Map               as Map
import           Servant.API            ( (:<|>), (:>), Capture,
                                          DeleteNoContent, Get, JSON, NoContent,
                                          PostNoContent, Raw, ReqBody, Post )
import           Servant.API.WebSocket  ( WebSocket )
import qualified Telescope.Table        as Table'

type TableKey = String
type Table    = (TableKey, [(Table'.RowKey, Table'.Row)])
type Tables   = [Table]

type RestAPI =
  "viewTables"
    :> ReqBody         '[JSON] [TableKey]
    :> Post            '[JSON] Tables
  :<|>
  "setRows"
    :> ReqBody         '[JSON] Tables
    :> PostNoContent   '[JSON] NoContent
  :<|>
  "setTables"
    :> ReqBody         '[JSON] Tables
    :> PostNoContent   '[JSON] NoContent
  :<|>
  "rmTables"
    :> ReqBody         '[JSON] [TableKey]
    :> DeleteNoContent '[JSON] NoContent

type WebSocketAPI = "watch" :> WebSocket
type StaticAPI    = Raw
type API          = RestAPI :<|> WebSocketAPI :<|> StaticAPI

--------------------------
-- CONVERSION FUNCTIONS --
--------------------------

-- | Convert Telescope Tables to API-format.
toAPITables :: Table'.Tables -> Tables
toAPITables = map f . Map.toList
  where f (Table'.TableKey tn, table) = (tn, Map.toList table)

-- | Convert from API-format to Telescope Tables.
fromAPITables :: Tables -> Table'.Tables
fromAPITables = Map.fromList . map f
  where f (tn, table) = (Table'.TableKey tn, Map.fromList table)

-- | Convert Telescope TableKeys to API-format.
toAPITableKeys :: [Table'.TableKey] -> [TableKey]
toAPITableKeys = map Table'.unTableKey

-- | Convert from API-format to Telescope TableKeys.
fromAPITableKeys :: [TableKey] -> [Table'.TableKey]
fromAPITableKeys = map Table'.TableKey
