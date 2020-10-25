{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Operations on entities in a data source.
module Telescope.Ops where

import           Data.Functor      ( (<&>) )
import           Control.Monad     ( join, when )
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

-- | Like 'view' but row key is passed separately.
viewK :: (Entity a, PrimaryKey a k, Telescope m f)
  => a -> k -> m (Maybe a)
viewK a primaryKey =
  viewKRx (Class.toF a) (Class.toF primaryKey)
  >>= pure . Class.fromF

-- | Like 'viewK', but a reactive version.
viewKRx :: (Entity a, PrimaryKey a k, Telescope m f)
  => f a -> f k -> m (f (Maybe a))
viewKRx a primaryKey =
  Class.viewRow
  (Table.tableKey <$> a)
  (Table.RowKey . Table.toKey <$> primaryKey)
  >>= pure . (fmap $ fmap Store.fromRow)

-- | View all entities in a table in a data source.
viewTable :: (Entity a, Telescope m f) => a -> m [a]
viewTable a = Class.fromF <$> (viewTableRx $ Class.toF a)

-- | Like 'viewTable', but a reactive version.
viewTableRx :: (Entity a, Telescope m f) => f a -> m (f [a])
viewTableRx a =
  (Class.viewTableRows $ Table.tableKey <$> a)
  >>= pure . (fmap $ fmap Store.fromRow . Map.elems)

-- | Set an entity in a data source.
--
-- WARNING: overwrites existing entity with same primary key.
set :: (Entity a, Telescope m f) => a -> m ()
set a = setRx $ Class.toF a

-- | Infix version of 'set'.
(.~) :: (Entity a, Telescope m f) => a -> m ()
(.~) = set

-- | Like 'set', but a reactive version.
setRx :: (Entity a, Telescope m f) => f a -> m ()
setRx a = Class.setManyRows $ Store.toRows . Store.toSDataType <$> a

-- | Set a table in a data source to ONLY the given entities.
--
-- WARNING: overwrites all existing entities in the table.
setTable :: (Entity a, Telescope m f) => [a] -> m ()
setTable = setTableRx . Class.toF

-- | Set a table in a data source to ONLY the given entities.
--
-- WARNING: overwrites all existing entities in the table.
-- TODO: return either, handling error case of duplicate rows.
-- TODO: use 'setTable' for values of type 'a'.
-- TODO: use 'setMany' only for values not of type 'a'.
setTableRx :: (Entity a, Telescope m f) => f [a] -> m ()
setTableRx as = Class.setManyRows tableMap
  where rowsPerA = fmap (map $ Store.toRows . Store.toSDataType) as
        tableMap = fmap (Map.unionsWith Map.union) rowsPerA

-- | Modify an entity in a data source.
over :: (Entity a, PrimaryKey a k, Telescope m f)
  => a -> (a -> a) -> m (Maybe a)
over a f = overK a (Table.primaryKey a) f

-- | Like 'over', but row key is passed separately.
overK :: (Entity a, PrimaryKey a k, Telescope m f)
  => a -> k -> (a -> a) -> m (Maybe a)
overK aType primaryKey f =
  overKRx (Class.toF aType) (Class.toF primaryKey) (Class.toF f)
  >>= pure . Class.fromF

-- | Like 'overK', but a reactive version.
overKRx :: forall a k f m. (Entity a, PrimaryKey a k, Telescope m f)
  => f a -> f k -> f (a -> a) -> m (f (Maybe a))
overKRx aTypeF primaryKeyF funcF = do
  oldAMayF <- viewKRx aTypeF primaryKeyF
      -- Modify the function to return (old value, new value)..
  let funcF' :: f (a -> (a, a))
      funcF' = (\func a -> (a, func a)) <$> funcF
      -- ..and apply this function over the "viewed" entity.
      oldNewAMayF :: f (Maybe (a, a))
      oldNewAMayF = fmap <$> funcF' <*> oldAMayF
  -- Whenever the entity changes, apply the function and "set" the update. In
  -- addition, if the primary key has changed, the old entity is "rm"ed.
  _ <- join $ Class.escape $ oldNewAMayF <&> \case
    Nothing           -> pure ()
    Just (oldA, newA) -> do
      when (Table.rowKey oldA /= Table.rowKey newA) $ rm oldA
      set newA
  pure $ fmap snd <$> oldNewAMayF

-- | Remove an entity in a data source.
rm :: (Entity a, PrimaryKey a k, Telescope m f) => a -> m ()
rm a = rmK a $ Table.primaryKey a

-- | Like 'rm' but row key is passed separately.
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
