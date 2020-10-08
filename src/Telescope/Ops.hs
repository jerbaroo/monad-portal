{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

-- * Operations on entities in a data source.
module Telescope.Ops where

import           Control.Monad    ( when )
import qualified Data.Map        as Map
import           Telescope.Class  ( Entity, PKey, Telescope )
import qualified Telescope.Class as Class
import qualified Telescope.Table as Table
import qualified Telescope.Store as Store

-- * View entities in a data source.

-- | View an entity in a table in a data source.
--
-- Table key and row key are derived from the value.
-- TODO: make 'PKey' a constraint on 'Entity' and use 'viewR' here.
view :: (Entity a, Telescope m) => a -> m (Maybe a)
view a =
  Class.view (Table.tableKey a) (Table.rowKey a) >>=
    pure . fmap Store.fromRow

-- | Infix version of 'view'.
(^.) :: (Entity a, Telescope m) => a -> m (Maybe a)
(^.) = view

-- | View an entity in a table in a data source, passing row key separately.
--
-- Example usage: viewR Person{} "john"
viewK :: (Entity a, PKey a k, Telescope m) => a -> k -> m (Maybe a)
viewK a pk =
  Class.view (Table.tableKey a) (Table.RowKey $ Table.toKey pk) >>=
    pure . fmap Store.fromRow

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

over :: (Entity a, PKey a k, Telescope m) => a -> (a -> a) -> m (Maybe a)
over a f = overK a (Table.pKey a) f

-- | Modify an entity in a data source, passing primary key separately.
overK :: (Entity a, PKey a k, Telescope m) => a -> k -> (a -> a) -> m (Maybe a)
overK aType pKey f = viewK aType pKey >>= \case
  Nothing -> pure Nothing
  Just a  -> do
    -- Remove the old value if the primary key has changed.
    when (pKey /= Table.pKey (f a)) $ rmK aType pKey
    set $ f a
    pure $ Just $ f a

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
