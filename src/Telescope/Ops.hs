{-# LANGUAGE TypeFamilies   #-}

module Telescope.Ops where

import qualified Data.Key        as Key
import           Telescope.Class  ( Entity, PKey, Telescope(..) )
import qualified Telescope.Table as Table
import qualified Telescope.Store as Store

-- * View

-- | View an entity in a data source.
-- TODO: decode 'Row' using Generics e.g. Row -> a.
--
-- Table key and row key are derived from the value.
view :: (Entity a, Telescope m) => a -> m (Maybe Table.Row)
view a = viewTele (Table.tableKey a) (Table.rowKey a)

-- | View an entity in a data source, passing row key separately.
-- TODO: decode 'Row' using Generics e.g. Row -> a.
--
-- Example usage: viewR Person{} "johnrooney"
viewR :: (Entity a, PKey a k, Telescope m) => a -> k -> m (Maybe Table.Row)
viewR a pk = viewTele (Table.tableKey a) (Table.RowKey $ Table.toKey pk)

-- | Set an entity in a data source.
set :: (Entity a, Telescope m) => a -> m ()
set a =
  Key.forWithKeyM_ (Store.toRows $ Store.toSDataType a) $ \tableKey table ->
    Key.forWithKeyM_ table $ \rowKey row ->
      setTele tableKey rowKey row
