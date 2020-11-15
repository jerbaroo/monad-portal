{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- Operations on entities in a data source.
module Telescope.Operations where

import           Control.Comonad          ( Comonad, extract )
import           Data.Proxy               ( Proxy(..) )
import qualified Data.Map                as Map
import           Telescope.Class          ( Entity, Telescope )
import qualified Telescope.Class         as Class
import qualified Telescope.Convert       as Convert
import qualified Telescope.Table.To      as Table
import qualified Telescope.Table.Types   as Table

----------
-- view --
----------

-- | View an entity in a data source.
view :: (Entity a k, Telescope m f, Comonad f) => a -> m (Maybe a)
view = viewK . Table.primaryKey

-- | Infix version of 'view'.
(^.) :: (Entity a k, Telescope m f, Comonad f) => a -> m (Maybe a)
(^.) = view

-- | Like 'view' but a reactive version.
viewRx :: (Entity a k, Telescope m f) => f a -> m (f (Maybe a))
viewRx aF = viewKRx $ Table.primaryKey <$> aF

-- | Like 'view' but row key is passed separately.
viewK :: (Entity a k, Telescope m f, Comonad f) => k -> m (Maybe a)
viewK primaryKey = pure . extract =<< viewKRx (pure primaryKey)

-- | Like 'viewK' but a reactive version.
viewKRx :: forall a k m f. (Entity a k, Telescope m f) => f k -> m (f (Maybe a))
viewKRx primaryKeyF = do
  Class.viewRow
    (pure $ Table.tableKey @a)
    (Table.RowKey . Table.toKey <$> primaryKeyF)
  >>= pure . (fmap $ fmap Convert.aFromRow)

-- | View all entities in a table in a data source.
viewTable :: forall a k m f. (Entity a k, Telescope m f, Comonad f) => m [a]
viewTable = extract <$> viewTableRx (pure $ Proxy @a)

-- | Like 'viewTable' but a reactive version.
viewTableRx :: forall a k m f. (Entity a k, Telescope m f)
  => f (Proxy a) -> m (f [a])
viewTableRx proxyF =
  Class.viewTable (const (Table.tableKey @a) <$> proxyF)
  >>= pure . (fmap $ fmap Convert.aFromRow . Map.elems)

---------
-- set --
---------

-- | Set an entity in a data source.
set :: (Entity a k, Telescope m f) => a -> m ()
set = setRx . pure

-- | Infix version of 'set'.
(.~) :: (Entity a k, Telescope m f) => a -> m ()
(.~) = set

-- | Like 'set' but a reactive version.
setRx :: (Entity a k, Telescope m f) => f a -> m ()
setRx aF = Class.setRows $ Convert.aToRows <$> aF

-- | Set a table in a data source to ONLY the given entities.
--
-- WARNING: removes all existing entities in the table.
--
-- WARNING: if multiple 'a's with equal primary keys are given, then only the
-- 'a' nearest the tail of the list will be set in the data source. And if any
-- 'a's contain nested 'Entity's with equal primary keys, then only the 'Entity'
-- contained within the 'a' nearest the tail of the list will be set.
setTable :: (Entity a k, Telescope m f) => [a] -> m ()
setTable = setTableRx . pure

-- | Like 'setTable' but a reactive version.
--
-- WARNING: removes all existing entities in the table.
--
-- WARNING: if multiple 'a's with equal primary keys are given, then only the
-- 'a' nearest the tail of the list will be set in the data source. And if any
-- 'a's contain nested 'Entity's with equal primary keys, then only the 'Entity'
-- contained within the 'a' nearest the tail of the list will be set.
setTableRx :: forall a k m f. (Entity a k, Telescope m f) => f [a] -> m ()
setTableRx asF = do
  -- Convert each 'a' into 1 or more rows across 1 or more tables and then
  -- combine these rows per 'a' into a single data structure 'tableMap'.
  let tableMap = Map.unionsWith Map.union . map Convert.aToRows <$> asF
  -- Set the table that was requested to be set..
  Class.setTable (pure $ Table.tableKey @a) $ maybe Map.empty id <$>
    (Map.lookup (Table.tableKey @a) <$> tableMap)
  -- ..and set any rows in any other tables.
  Class.setRows $ Map.delete (Table.tableKey @a) <$> tableMap

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

-- -- | Like 'overK' but a reactive version.
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
rm :: forall a k m f. (Entity a k, Telescope m f) => a -> m ()
rm = rmK @a . Table.primaryKey

-- | Like 'rm' but a reactive version.
rmRx :: forall a k m f. (Entity a k, Telescope m f) => f a -> m ()
rmRx aF = rmKRx @a $ Table.primaryKey <$> aF

-- | Like 'rm' but row key is passed separately.
rmK :: forall a k m f. (Entity a k, Telescope m f) => k -> m ()
rmK = rmKRx @a . pure

-- | Like 'rmK' but a reactive version.
rmKRx :: forall a k m f. (Entity a k, Telescope m f) => f k -> m ()
rmKRx primaryKeyF =
  Class.rmRow
    (pure $ Table.tableKey @a)
    (Table.RowKey . Table.toKey <$> primaryKeyF)

-- | Remove a table in a data source.
rmTable :: forall a k m f. (Entity a k, Telescope m f) => m ()
rmTable = rmTableRx $ pure $ Proxy @a

-- | Like 'rmTable' but a reactive version.
rmTableRx :: forall a k m f. (Entity a k, Telescope m f) => f (Proxy a) -> m ()
rmTableRx proxyF = Class.rmTable $ const (Table.tableKey @a) <$> proxyF

--------------
-- onChange --
--------------

-- | Run a function when an entity in a data source changes.
onChange :: forall a k m f. (Entity a k, Telescope m f)
  => k -> (Maybe a -> m ()) -> m ()
onChange k f = onChangeRx @a (pure k) (pure f)

-- | Like 'onChange' but a reacive version.
onChangeRx :: forall a k m f. (Entity a k, Telescope m f) =>
  f k -> f (Maybe a -> m ()) -> m ()
onChangeRx kF fF =
  Class.onChangeRow
    (pure $ Table.tableKey @a)
    (Table.RowKey . Table.toKey <$> kF)
    ((. fmap Convert.aFromRow) <$> fF)
