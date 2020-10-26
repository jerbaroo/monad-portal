{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Telescope.Server.API where

import           Data.Aeson             ( FromJSON, ToJSON )
import           Servant.API            ( (:<|>), (:>), Capture,
                                          DeleteNoContent, Get, JSON, NoContent,
                                          PostNoContent, Raw, ReqBody )
import           Servant.API.WebSocket  ( WebSocket )
import           Telescope.Table       as Table

type TableAsList = [(Table.RowKey, Table.Row)]

instance FromJSON Table.ColumnKey
instance FromJSON Table.Key
instance FromJSON Table.Prim
instance FromJSON Table.RowKey

instance ToJSON Table.ColumnKey
instance ToJSON Table.Key
instance ToJSON Table.Prim
instance ToJSON Table.RowKey

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
