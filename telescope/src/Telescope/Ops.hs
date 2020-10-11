{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

-- * Operations on entities in a data source.
module Telescope.Ops where

import           Control.Monad    ( when )
import qualified Data.Map        as Map
import           Telescope.Class  ( Entity, PrimaryKey, Telescope )
import qualified Telescope.Class as Class
import qualified Telescope.Table as Table
import qualified Telescope.Store as Store

-- * View entities in a data source.

-- | View an entity in a data source.
--
-- Table key and row key are derived from the value.
view :: (Entity a, PrimaryKey a k, Telescope m) => a -> m (Maybe a)
view a = viewK a $ Table.primaryKey a

-- | Infix version of 'view'.
(^.) :: (Entity a, PrimaryKey a k, Telescope m) => a -> m (Maybe a)
(^.) = view

-- | View an entity in a data source, passing row key separately.
--
-- Example usage: viewR Person{} "john"
viewK :: (Entity a, PrimaryKey a k, Telescope m) => a -> k -> m (Maybe a)
viewK a primaryKey =
  Class.viewRow (Table.tableKey a) (Table.RowKey $ Table.toKey primaryKey) >>=
    pure . fmap Store.fromRow

-- | View all entities in a table in a data source.
viewTable :: (Entity a, Telescope m) => a -> m [a]
viewTable a = do
  (Class.viewTableRows $ Table.tableKey a) >>=
    pure . fmap Store.fromRow . Map.elems

-- * Set entities in a data source.

-- | Set an entity in a data source.
--
-- WARNING: overwrites existing entity with same primary key.
set :: (Entity a, Telescope m) => a -> m ()
set a = Class.setManyRows $ Store.toRows $ Store.toSDataType a

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
setTable as = Class.setManyRows tableMap
  where rowsPerA :: [Map.Map Table.TableKey Table.Table]
        rowsPerA = map (Store.toRows . Store.toSDataType) as
        tableMap :: Map.Map Table.TableKey Table.Table
        tableMap = Map.unionsWith Map.union rowsPerA

-- * Modify entities in a data source.

-- | Modify an entity in a data source.
over :: (Entity a, PrimaryKey a k, Telescope m) => a -> (a -> a) -> m (Maybe a)
over a f = overK a (Table.primaryKey a) f

-- | Modify an entity in a data source, passing primary key separately.
overK :: (Entity a, PrimaryKey a k, Telescope m)
  => a -> k -> (a -> a) -> m (Maybe a)
overK aType primaryKey f = viewK aType primaryKey >>= \case
  Nothing -> pure Nothing
  Just a  -> do
    -- Remove the old value if the primary key has changed.
    when (primaryKey /= Table.primaryKey (f a)) $ rmK aType primaryKey
    set $ f a
    pure $ Just $ f a

-- * Remove entities in a data source.

-- | Remove an entity in a data source.
rm :: (Entity a, PrimaryKey a k, Telescope m) => a -> m ()
rm a = rmK a $ Table.primaryKey a

-- | Remove an entity in a data source, passing row key separately.
rmK :: (Entity a, PrimaryKey a k, Telescope m) => a -> k -> m ()
rmK a primaryKey =
  Class.rmRow (Table.tableKey a) (Table.RowKey $ Table.toKey primaryKey)

-- | Remove a table in a data source.
rmTable :: (Entity a, Telescope m) => a -> m ()
rmTable a = Class.rmTableRows $ Table.tableKey a

-- * Watch for changes to entities in a data source.

-- | Run a function when an entity in a data source changes.
--
-- The function takes a 'Maybe' to indicate the entity may have been removed.
onChange :: (Entity a, Telescope m) => a -> (Maybe a -> m ()) -> m ()
onChange a f =
  Class.onChangeRow (Table.tableKey a) (Table.rowKey a) (f . fmap Store.fromRow)
  
