{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE QuantifiedConstraints  #-}

module Telescope.Class (module Telescope.Class, PrimaryKey) where

import           Control.Monad            ( void )
import           Data.Functor             ( (<&>) )
import           Data.Functor.Identity    ( Identity(..) )
import qualified Data.Map                as Map
import qualified Data.Set                as Set
import qualified Telescope.Store         as Store
import qualified Telescope.Table         as Table
import           Telescope.Table          ( PrimaryKey )

-- | 'Table.Row'-based operations for interacting with a data source.
-- TODO: Use Set instead of list for key types.
class (Applicative f, Monad m) => Telescope m f | m -> f where
  -- | View one row in a data source.
  viewRow :: f Table.TableKey -> f Table.RowKey -> m (f (Maybe Table.Row))
  viewRow tableKeyF rowKeyF = do
    rowsF <- viewRows $ Map.singleton <$> tableKeyF <*> ((:[]) <$> rowKeyF)
    pure $ Map.lookup <$> rowKeyF <*> ((Map.!) <$> rowsF <*> tableKeyF)

  -- | View multiple rows in a data source.
  viewRows
    :: f (Map.Map Table.TableKey [Table.RowKey])
    -> m (f (Map.Map Table.TableKey Table.Table))
  viewRows rowKeysMapF = do
    tablesF <- viewTables $ Map.keys <$> rowKeysMapF
    pure $ Map.intersectionWith onlyRows <$> tablesF <*> rowKeysMapF

  -- | View one table in a data source.
  viewTable :: f Table.TableKey -> m (f (Table.Table))
  viewTable tableKeyF = do
    tablesF <- viewTables $ (:[]) <$> tableKeyF
    pure $ (Map.!) <$> tablesF <*> tableKeyF

  -- | View multiple tables in a data source.
  viewTables :: f [Table.TableKey] -> m (f (Map.Map Table.TableKey Table.Table))

  -- | Set one row in a data source.
  setRow :: f Table.TableKey -> f Table.RowKey -> f Table.Row -> m ()
  setRow tableKeyF rowKeyF rowF =
    setRows $ Map.singleton <$> tableKeyF <*>
      (Map.singleton <$> rowKeyF <*> rowF)

  -- | Set multiple rows in a data source.
  setRows :: f (Map.Map Table.TableKey Table.Table) -> m ()
  setRows newRowsMapF = do
    tablesF <- viewTables $ Map.keys <$> newRowsMapF
    setTables $ Map.union <$> newRowsMapF <*> tablesF

  -- | Set one table in a data source.
  setTable :: f Table.TableKey -> f Table.Table -> m ()
  setTable tableKeyF tableF = setTables $ Map.singleton <$> tableKeyF <*> tableF

  -- | Set multiple tables in a data source.
  setTables :: f (Map.Map Table.TableKey Table.Table) -> m ()

  -- | Remove one row from a data source.
  rmRow :: f Table.TableKey -> f Table.RowKey -> m ()
  rmRow tableKeyF rowKeyF =
    rmRows $ Map.singleton <$> tableKeyF <*> ((:[]) <$> rowKeyF)

  -- | Remove multiple rows from a data source.
  rmRows :: f (Map.Map Table.TableKey [Table.RowKey]) -> m ()
  rmRows rowKeysMapF = do
    tablesF <- viewTables $ Map.keys <$> rowKeysMapF
    setTables $ Map.intersectionWith withoutRows <$> tablesF <*> rowKeysMapF

  -- | Remove one table from a data source.
  rmTable :: f Table.TableKey -> m ()
  rmTable tableKeyF = rmTables $ (:[]) <$> tableKeyF

  -- | Remove multiples table from a data source.
  rmTables :: f [Table.TableKey] -> m ()
  rmTables tableKeysF =
    setTables $ Map.fromList . (flip zip $ repeat Map.empty) <$> tableKeysF

  -- | Run a function when a row in a data source changes.
  onChangeRow
    :: f Table.TableKey -> f Table.RowKey -> f (Maybe Table.Row -> m ()) -> m ()

  perform :: f (m ()) -> m ()

-- | A storable datatype (can be serialized and deserialized via Generics).
class    (Store.ToSDataType a, Store.FromSValues a) => Entity a where
instance (Store.ToSDataType a, Store.FromSValues a) => Entity a where

-- | A 'Table.Table' with ONLY rows corresponding to given 'Table.RowKey's.
onlyRows :: Table.Table -> [Table.RowKey] -> Table.Table
onlyRows table rowKeys = Map.restrictKeys table (Set.fromList rowKeys)

-- | A 'Table.Table' WITHOUT rows corresponding to given 'Table.RowKey's.
withoutRows :: Table.Table -> [Table.RowKey] -> Table.Table
withoutRows table rowKeys = Map.withoutKeys table (Set.fromList rowKeys)

-- | A container type that can be entered/exited.
class ToFromF f where
  toF   :: a -> f a
  fromF :: f a -> a

-- | Data sources may pack values in the identity functor.
instance ToFromF Identity where
  toF   = Identity
  fromF = runIdentity
