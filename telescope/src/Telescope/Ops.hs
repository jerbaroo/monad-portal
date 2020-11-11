{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Operations on entities in a data source.
module Telescope.Ops where

import qualified Data.Map        as Map
import           Telescope.Class  ( Entity, PrimaryKey, Telescope, ToFromF )
import qualified Telescope.Class as Class
import qualified Telescope.Table as Table
import qualified Telescope.Store as Store

----------
-- view --
----------

-- | View an entity in a data source.
view :: (Entity a, PrimaryKey a k, Telescope m f, ToFromF f) => a -> m (Maybe a)
view a = viewK a $ Table.primaryKey a

-- | Infix version of 'view'.
(^.) :: (Entity a, PrimaryKey a k, Telescope m f, ToFromF f) => a -> m (Maybe a)
(^.) = view

-- | Like 'view' but a reactive version.
viewRx :: (Entity a, PrimaryKey a k, Telescope m f) => f a -> m (f (Maybe a))
viewRx aF = viewKRx aF (Table.primaryKey <$> aF)

-- | Like 'view' but row key is passed separately.
viewK :: (Entity a, PrimaryKey a k, Telescope m f, ToFromF f)
  => a -> k -> m (Maybe a)
viewK a primaryKey =
  viewKRx (Class.toF a) (Class.toF primaryKey)
  >>= pure . Class.fromF

-- | Like 'viewK' but a reactive version.
viewKRx :: (Entity a, PrimaryKey a k, Telescope m f)
  => f a -> f k -> m (f (Maybe a))
viewKRx aF primaryKeyF =
  Class.viewRow
  (Table.tableKey <$> aF)
  (Table.RowKey . Table.toKey <$> primaryKeyF)
  >>= pure . (fmap $ fmap Store.fromRow)

-- | View all entities in a table in a data source.
viewTable :: (Entity a, Telescope m f, ToFromF f) => a -> m [a]
viewTable a = Class.fromF <$> (viewTableRx $ Class.toF a)

-- | Like 'viewTable' but a reactive version.
viewTableRx :: (Entity a, Telescope m f) => f a -> m (f [a])
viewTableRx aF =
  (Class.viewTable $ Table.tableKey <$> aF)
  >>= pure . (fmap $ fmap Store.fromRow . Map.elems)

---------
-- set --
---------

-- | Set an entity in a data source.
set :: (Entity a, Telescope m f, ToFromF f) => a -> m ()
set a = setRx $ Class.toF a

-- | Infix version of 'set'.
(.~) :: (Entity a, Telescope m f, ToFromF f) => a -> m ()
(.~) = set

-- | Like 'set' but a reactive version.
setRx :: (Entity a, Telescope m f) => f a -> m ()
setRx aF = Class.setRows $ Store.toRows . Store.toSDataType <$> aF

-- | Set a table in a data source to ONLY the given entities.
--
-- WARNING: removes all existing entities in the table.
setTable :: (Entity a, Telescope m f, ToFromF f) => [a] -> m ()
setTable = setTableRx . Class.toF

-- | Like 'setTable' but a reactive version.
--
-- WARNING: removes all existing entities in the table.
-- TODO: return either, handling error case of duplicate rows.
setTableRx :: (Entity a, Telescope m f) => f [a] -> m ()
setTableRx asF = do
  let tableKeyF = Table.tableKey <$> asF
      rowsPerA  = (map $ Store.toRows . Store.toSDataType) <$> asF
      tableMap  = Map.unionsWith Map.union <$> rowsPerA
  Class.setTable tableKeyF $ (maybe Map.empty id)
    <$> (Map.lookup <$> tableKeyF <*> tableMap)
  Class.setRows $ Map.delete <$> tableKeyF <*> tableMap

----------
-- over --
----------

-- -- | Modify an entity in a data source.
-- over :: (Entity a, PrimaryKey a k, Telescope m f)
--   => a -> (a -> a) -> m (Maybe a)
-- over a f = overK a (Table.primaryKey a) f

-- -- | Like 'over' but a reactive version.
-- overRx :: (Entity a, PrimaryKey a k, Telescope m f)
--   => f a -> f (a -> a) -> m (f (Maybe a))
-- overRx aF fF = overKRx aF (Table.primaryKey <$> aF) fF

-- -- | Like 'over' but row key is passed separately.
-- overK :: (Entity a, PrimaryKey a k, Telescope m f)
--   => a -> k -> (a -> a) -> m (Maybe a)
-- overK aType primaryKey f =
--   overKRx (Class.toF aType) (Class.toF primaryKey) (Class.toF f)
--   >>= pure . Class.fromF

-- | Like 'overK' but a reactive version.
-- overKRx :: forall a k f m. (Entity a, PrimaryKey a k, Telescope m f)
--   => f a -> f k -> f (a -> a) -> m (f (Maybe a))
-- overKRx aTypeF primaryKeyF funcF = do
--   oldAMayF <- viewKRx aTypeF primaryKeyF
--       -- Modify the function to return (old value, new value)..
--   let funcF' :: f (a -> (a, a))
--       funcF' = (\func a -> (a, func a)) <$> funcF
--       -- ..and apply this function over the "viewed" entity.
--       oldNewAMayF :: f (Maybe (a, a))
--       oldNewAMayF = fmap <$> funcF' <*> oldAMayF
--   -- Whenever the entity changes, apply the function and "set" the update. In
--   -- addition, if the primary key has changed, the old entity is "rm"ed.
--   _ <- join $ Class.escape $ oldNewAMayF <&> \case
--     Nothing           -> pure ()
--     Just (oldA, newA) -> do
--       when (Table.rowKey oldA /= Table.rowKey newA) $ rm oldA
--       set newA
--   pure $ fmap snd <$> oldNewAMayF

--------
-- rm --
--------

-- | Remove an entity in a data source.
rm :: (Entity a, PrimaryKey a k, Telescope m f, ToFromF f) => a -> m ()
rm a = rmK a $ Table.primaryKey a

-- | Like 'rm' but a reactive version.
rmRx :: (Entity a, PrimaryKey a k, Telescope m f) => f a -> m ()
rmRx aF = rmKRx aF (Table.primaryKey <$> aF)

-- | Like 'rm' but row key is passed separately.
rmK :: (Entity a, PrimaryKey a k, Telescope m f, ToFromF f) => a -> k -> m ()
rmK a primaryKey = rmKRx (Class.toF a) (Class.toF primaryKey)

-- | Like 'rmK' but a reactive version.
rmKRx :: (Entity a, PrimaryKey a k, Telescope m f) => f a -> f k -> m ()
rmKRx aF primaryKeyF =
  Class.rmRow
  (Table.tableKey <$> aF)
  (Table.RowKey . Table.toKey <$> primaryKeyF)

-- | Remove a table in a data source.
rmTable :: (Entity a, Telescope m f, ToFromF f) => a -> m ()
rmTable = rmTableRx . Class.toF

-- | Like 'rmTable' but a reactive version.
rmTableRx :: (Entity a, Telescope m f) => f a -> m ()
rmTableRx aF = Class.rmTable $ Table.tableKey <$> aF

--------------
-- onChange --
--------------

-- | Run a function when an entity in a data source changes.
onChange :: (Entity a, Telescope m f, ToFromF f) => a -> (Maybe a -> m ()) -> m ()
onChange a f = onChangeRx (Class.toF a) (Class.toF f)

-- | Like 'onChange' but a reacive version.
onChangeRx :: (Entity a, Telescope m f) => f a -> f (Maybe a -> m ()) -> m ()
onChangeRx aF fF =
  Class.onChangeRow
  (Table.tableKey <$> aF)
  (Table.rowKey   <$> aF)
  ((. fmap Store.fromRow) <$> fF)
