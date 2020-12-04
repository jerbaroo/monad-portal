{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

-- Operations on entities in a data source.
module Telescope.Operations where

import           Control.Bool             ( guard' )
import           Control.Comonad          ( extract )
import           Control.Monad            ( join, when )
import           Data.Proxy               ( Proxy(Proxy) )
import qualified Data.Map                as Map
import           Data.Witherable          ( Filterable )
import qualified Data.Witherable         as Witherable
import           Telescope.Class          ( Box, Entity, Telescope )
import qualified Telescope.Class         as Class
import qualified Telescope.Convert       as Convert
import qualified Telescope.Table.To      as Table

--------------------------------------------------------------------------------
-- view ------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | View an entity in a data source.
view :: (Entity a k, Telescope m f, Box f) => a -> m (Maybe a)
view = viewK . Table.primaryKey

-- | Infix version of 'view'.
(^.) :: (Entity a k, Telescope m f, Box f) => a -> m (Maybe a)
(^.) = view

-- | Like 'view' but a reactive version.
viewRx :: (Entity a k, Telescope m f) => f a -> m (f (Maybe a))
viewRx aF = viewKRx $ Table.primaryKey <$> aF

-- | Like 'view' but row key is passed separately.
viewK :: (Entity a k, Telescope m f, Box f) => k -> m (Maybe a)
viewK primaryKey = pure . extract =<< viewKRx (Class.box primaryKey)

-- | Like 'viewK' but a reactive version.
viewKRx :: forall a k m f. (Entity a k, Telescope m f) => f k -> m (f (Maybe a))
viewKRx primaryKeyF = do
  Class.viewRow $ (Table.tableKey @a,) . Table.toRowKey <$> primaryKeyF
  >>= pure . (fmap $ fmap Convert.aFromRow)

-- | View all entities in a table in a data source.
viewTable :: forall a k m f. (Entity a k, Telescope m f, Box f) => m [a]
viewTable = extract <$> viewTableRx (Class.box $ Proxy @a)

-- | Like 'viewTable' but a reactive version.
viewTableRx :: forall a k m f. (Entity a k, Telescope m f)
  => f (Proxy a) -> m (f [a])
viewTableRx proxyF =
  Class.viewTable (const (Table.tableKey @a) <$> proxyF)
  >>= pure . (fmap $ fmap Convert.aFromRow . Map.elems)

--------------------------------------------------------------------------------
-- set -------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Set an entity in a data source.
set :: (Entity a k, Telescope m f, Box f) => a -> m ()
set = setRx . Class.box

-- | Infix version of 'set'.
(.~) :: (Entity a k, Telescope m f, Box f) => a -> m ()
(.~) = set

-- | Like 'set' but a reactive version.
setRx :: (Entity a k, Telescope m f) => f a -> m ()
setRx aF = Class.setRows $ Convert.aToRows <$> aF

-- | Set a table in a data source to ONLY the given entities.
--
-- WARNING: removes all existing entities in the table.
--
-- WARNING: if multiple 'a's with equal primary keys are given, then only the
-- 'a' nearest the head of the list will be set in the data source. And if any
-- 'a's contain nested 'Entity's with equal primary keys, then only the 'Entity'
-- contained within the 'a' nearest the head of the list will be set.
setTable :: (Entity a k, Telescope m f, Box f) => [a] -> m ()
setTable = setTableRx . Class.box

-- | Like 'setTable' but a reactive version.
--
-- WARNING: removes all existing entities in the table.
--
-- WARNING: if multiple 'a's with equal primary keys are given, then only the
-- 'a' nearest the head of the list will be set in the data source. And if any
-- 'a's contain nested 'Entity's with equal primary keys, then only the 'Entity'
-- contained within the 'a' nearest the head of the list will be set.
setTableRx :: forall a k m f. (Entity a k, Telescope m f) => f [a] -> m ()
setTableRx asF = do
  -- Convert each 'a' into 1 or more rows across 1 or more tables and then
  -- combine these rows per 'a' into a single data structure 'tableMap'.
  let tableMap = Map.unionsWith Map.union . map Convert.aToRows <$> asF
  -- Set the table corresponding to type 'a'..
  Class.setTable $
    (\tableMay -> (Table.tableKey @a, maybe Map.empty id tableMay))
    <$> (Map.lookup (Table.tableKey @a) <$> tableMap)
  -- ..and set any rows in any other tables.
  Class.setRows $ Map.delete (Table.tableKey @a) <$> tableMap

--------------------------------------------------------------------------------
-- over ------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Apply a function over an entity in a data source.
over :: (Entity a k, Telescope m f, Box f) => a -> (a -> a) -> m (Maybe a)
over a func = overK (Table.primaryKey a) func

-- | Like 'over' but a reactive version.
overRx :: (Entity a k, Telescope m f, Filterable f) =>
  f a -> (a -> a) -> m (f (Maybe a))
overRx aF func = overKRx (Table.primaryKey <$> aF) func

-- | Like 'over' but row key is passed separately.
overK :: forall a k m f. (Entity a k, Telescope m f, Box f) =>
  k -> (a -> a) -> m (Maybe a)
overK primaryKey func = do
  oldAMay <- viewK primaryKey
  case oldAMay of
    Nothing   -> pure Nothing
    Just oldA -> do
      when (Table.rowKey oldA /= Table.rowKey (func oldA)) $ rm oldA
      set  $ func oldA
      pure $ Just $ func oldA

-- | Like 'overK' but a reactive version.
overKRx :: forall a k m f. (Entity a k, Telescope m f, Filterable f)
  => f k -> (a -> a) -> m (f (Maybe a))
overKRx primaryKeyF func = do
  oldAMayF <- viewKRx primaryKeyF
  let newAMayF  = fmap func <$> oldAMayF
      toRmMay a = guard' (Table.rowKey a == Table.rowKey (func a)) a
      rmAMayF   = Witherable.catMaybes $ join . fmap toRmMay <$> oldAMayF
  setRx $ Witherable.catMaybes newAMayF
  rmRx rmAMayF
  pure newAMayF

--------------------------------------------------------------------------------
-- rm --------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Remove an entity in a data source.
rm :: forall a k m f. (Entity a k, Telescope m f, Box f) => a -> m ()
rm = rmK @a . Table.primaryKey

-- | Like 'rm' but a reactive version.
rmRx :: forall a k m f. (Entity a k, Telescope m f) => f a -> m ()
rmRx aF = rmKRx @a $ Table.primaryKey <$> aF

-- | Like 'rm' but row key is passed separately.
rmK :: forall a k m f. (Entity a k, Telescope m f, Box f) => k -> m ()
rmK = rmKRx @a . Class.box

-- | Like 'rmK' but a reactive version.
rmKRx :: forall a k m f. (Entity a k, Telescope m f) => f k -> m ()
rmKRx primaryKeyF =
  Class.rmRow $ (Table.tableKey @a,) . Table.toRowKey <$> primaryKeyF

-- | Remove a table in a data source.
rmTable :: forall a k m f. (Entity a k, Telescope m f, Box f) => m ()
rmTable = rmTableRx $ Class.box $ Proxy @a

-- | Like 'rmTable' but a reactive version.
rmTableRx :: forall a k m f. (Entity a k, Telescope m f) => f (Proxy a) -> m ()
rmTableRx proxyF = Class.rmTable $ const (Table.tableKey @a) <$> proxyF

--------------------------------------------------------------------------------
-- onChange --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Run a function when an entity in a data source changes.
onChange :: forall a k m f. (Entity a k, Telescope m f, Box f)
  => k -> (Maybe a -> m ()) -> m ()
onChange primaryKey f = onChangeRx @a (Class.box primaryKey) (Class.box f)

-- | Like 'onChange' but a reacive version.
onChangeRx :: forall a k m f. (Entity a k, Telescope m f) =>
  f k -> f (Maybe a -> m ()) -> m ()
onChangeRx primaryKeyF fF =
  Class.onChangeRow
    ((Table.tableKey @a,) . Table.toRowKey <$> primaryKeyF)
    ((. fmap Convert.aFromRow) <$> fF)
