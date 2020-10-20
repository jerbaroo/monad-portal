{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Operations on entities in a data source.
module Telescope.Ops where

import           Data.Functor      ( (<&>) )
import           Control.Monad     ( void, when )
import qualified Data.Map        as Map
import           Telescope.Class  ( Entity, PrimaryKey, Telescope )
import qualified Telescope.Class as Class
import qualified Telescope.Table as Table
import qualified Telescope.Store as Store

-- | View an entity in a data source.
view :: (Entity a, PrimaryKey a k, Telescope m f) => a -> m (Maybe a)
view a = viewK a $ Table.primaryKey a

-- | Infix version of 'view'.
(^.) :: (Entity a, PrimaryKey a k, Telescope m f) => a -> m (Maybe a)
(^.) = view

-- | Like 'view' but passing row key separately.
viewK :: (Entity a, PrimaryKey a k, Telescope m f)
  => a -> k -> m (Maybe a)
viewK a primaryKey =
  Class.viewRow
  (Table.tableKey <$> a)
  (Table.RowKey . Table.toKey <$> primaryKey) >>=
    pure . (fmap $ fmap Store.fromRow)

-- | View all entities in a table in a data source.
viewTable :: (Entity a, Telescope m f) => f a -> m (f [a])
viewTable a = do
  (Class.viewTableRows $ Table.tableKey <$> a) >>=
    pure . (fmap $ fmap Store.fromRow . Map.elems)

-- | Set an entity in a data source.
--
-- WARNING: overwrites existing entity with same primary key.
set :: (Entity a, Telescope m f) => f a -> m ()
set a = Class.setManyRows $ Store.toRows . Store.toSDataType <$> a

-- | Infix version of 'set'.
(.~) :: (Entity a, Telescope m f) => f a -> m ()
(.~) = set

-- | Set a table in a data source to ONLY the given entities.
--
-- WARNING: overwrites all existing entities in the table.
-- TODO: return either, handling error case of duplicate rows.
-- TODO: use 'setTable' for values of type 'a'.
-- TODO: use 'setMany' only for values not of type 'a'.
setTable :: (Entity a, Telescope m f) => f [a] -> m ()
setTable as = Class.setManyRows tableMap
  where rowsPerA = fmap (map $ Store.toRows . Store.toSDataType) as
        tableMap = fmap (Map.unionsWith Map.union) rowsPerA

-- | Modify an entity in a data source.
over :: (Entity a, PrimaryKey a k, Telescope m f)
  => a -> (Maybe a -> Maybe a) -> m (Maybe a)
over a f = overK a (Table.primaryKey a) f

-- | Like 'over' but passing primary key separately.
overK :: (Entity a, PrimaryKey a k, Telescope m f)
  => a -> k -> (Maybe a -> Maybe a) -> m (Maybe a)
overK aType primaryKey f =
  pure . Class.fromF
  =<< overKRx (Class.toF aType) (Class.toF primaryKey) (Class.toF f)

-- | Modify an entity in a data source, passing primary key separately.
overKRx :: forall a k f m. (Entity a, PrimaryKey a k, Telescope m f)
  => f a -> f k -> f (Maybe a -> Maybe a) -> m (f (Maybe a))
overKRx aType primaryKey f = do
  fMaybeA <- viewK aType primaryKey
  _ <- Class.escape $ f <*> fMaybeA <&> \newMaybeA -> do
    case newMaybeA of
      Nothing   -> pure ()
      Just newA -> do
          fNewA <- Class.enter newA
          set fNewA :: m ()
  pure $ fMaybeA

-- | Remove an entity in a data source.
rm :: (Entity a, PrimaryKey a k, Telescope m f) => a -> m ()
rm a = rmK a $ Table.primaryKey a

-- | Like 'rm' but passing row key separately.
rmK :: (Entity a, PrimaryKey a k, Telescope m f) => a -> k -> m ()
rmK a primaryKey =
  Class.rmRow
  (Class.toF $ Table.tableKey a)
  (Class.toF $ Table.RowKey $ Table.toKey primaryKey)

-- | Remove a table in a data source.
rmTable :: (Entity a, Telescope m f) => a -> m ()
rmTable a = Class.rmTableRows $ Class.toF $ Table.tableKey a

-- | Run a function when an entity in a data source changes.
onChange :: (Entity a, Telescope m f) => a -> (Maybe a -> m ()) -> m ()
onChange a f =
  Class.onChangeRow
  (Class.toF $ Table.tableKey a)
  (Class.toF $ Table.rowKey a)
  (Class.toF $ f . fmap Store.fromRow)
