{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Telescope.Server.API where

import           Data.Bifunctor         ( first, second )
import qualified Data.Map              as Map
import           Data.Set               ( Set )
import qualified Data.Set              as Set
import           Servant.API            ( (:<|>), (:>), JSON, NoContent,
                                          PostNoContent, Raw, ReqBody, Post )
import           Servant.API.WebSocket  ( WebSocket )
import qualified Telescope.Table       as Table'

type RowsIndex = [(TableKey, [Table'.RowKey])]
type TableKey  = String
type Table     = (TableKey, [(Table'.RowKey, Table'.Row)])
type Tables    = [Table]

type RestAPI =
  "viewRows"
    :> ReqBody       '[JSON] RowsIndex
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
    :> ReqBody       '[JSON] RowsIndex
    :> PostNoContent '[JSON] NoContent
  :<|>
  "rmTables"
    :> ReqBody       '[JSON] [TableKey]
    :> PostNoContent '[JSON] NoContent

type WebSocketAPI = "watch" :> WebSocket
type StaticAPI    = Raw
type API          = RestAPI :<|> WebSocketAPI :<|> StaticAPI

--------------------------
-- CONVERSION FUNCTIONS --
--------------------------

-- | A datatype that can be converted to/from API format.
class APIFormat a b | a -> b, b -> a where
  to   :: a -> b
  from :: b -> a

instance APIFormat Table'.RowsIndex RowsIndex where
  to   = map (first Table'.unTableKey . second Set.toList) . Map.toList
  from = Map.fromList . map (first Table'.TableKey . second Set.fromList)

instance APIFormat Table'.Tables Tables where
  to   = map (first Table'.unTableKey) . Map.toList . fmap Map.toList
  from = fmap Map.fromList . Map.fromList . map (first Table'.TableKey)

instance APIFormat (Set Table'.TableKey) [TableKey] where
  to   = map Table'.unTableKey . Set.toList
  from = Set.fromList . map Table'.TableKey
