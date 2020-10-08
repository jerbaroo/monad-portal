{-# LANGUAGE TypeFamilies   #-}

-- * Operations on entities in a data source.
module Telescope.Ops where

import qualified Data.Map        as Map
import           Telescope.Class  ( Entity, PKey, Telescope )
import qualified Telescope.Class as Class
import qualified Telescope.Table as Table
import qualified Telescope.Store as Store

-- * View entities in a data source.

-- | View an entity in a table in a data source.
-- TODO: decode 'Row' using Generics e.g. Row -> a.
--
-- Table key and row key are derived from the value.
view :: (Entity a, Telescope m) => a -> m (Maybe a)
view a = do
  ma <- Class.view (Table.tableKey a) (Table.rowKey a)
  pure $ Store.fromRow <$> ma

-- | Infix version of 'view'.
(^.) :: (Entity a, Telescope m) => a -> m (Maybe a)
(^.) = view

-- | View an entity in a table in a data source, passing row key separately.
-- TODO: decode 'Row' using Generics e.g. Row -> a.
--
-- Example usage: viewR Person{} "john"
viewR :: (Entity a, PKey a k, Telescope m) => a -> k -> m (Maybe Table.Row)
viewR a pk = Class.view (Table.tableKey a) (Table.RowKey $ Table.toKey pk)

-- | View all entities in a table in a data source.
viewTable :: (Entity a, Telescope m) => a -> m Table.Table
viewTable a = Class.viewTable $ Table.tableKey a

-- * Set entities in a data source.

-- | Set an entity in a data source.
--
-- WARNING: overwrites existing entity with same primary key.
set :: (Entity a, Telescope m) => a -> m ()
set a = Class.setMany $ Store.toRows $ Store.toSDataType a

-- | Infix version of 'set'.
(.~) :: (Entity a, Telescope m) => a -> m ()
(.~) = set

-- | Set a table in a data source to ONLY the given entities.
--
-- WARNING: overwrites all existing entities in the table.
-- TODO: return either, handling error case of duplicate rows.
-- TODO: use 'setTable' for values of type 'a'.
-- TODO: use 'setMany' only for values not of type 'a'.
setTable :: (Entity a, Telescope m) => [a] -> m ()
setTable as = Class.setMany tableMap
  where rowsPerA :: [Map.Map Table.TableKey Table.Table]
        rowsPerA = map (Store.toRows . Store.toSDataType) as
        tableMap :: Map.Map Table.TableKey Table.Table
        tableMap = Map.unionsWith Map.union rowsPerA

-- * Modify entities in a data source.

-- | Modify an entity in a data source.
-- TODO: this first requires decoding to work.
-- over :: (Entity a, Telescope m) => a -> (a -> a) ->

-- * Remove entities in a data source.

-- | Remove an entity in a table in a data source.
rm :: (Entity a, Telescope m) => a -> m ()
rm a = Class.rm (Table.tableKey a) (Table.rowKey a)

-- | Remove an entity in a table in a data source.
rmK :: (Entity a, PKey a k, Telescope m) => a -> k -> m ()
rmK a pk = Class.rm (Table.tableKey a) (Table.RowKey $ Table.toKey pk)

-- | Remove a table in a data source.
rmTable :: (Entity a, Telescope m) => a -> m ()
rmTable a = Class.rmTable $ Table.tableKey a
