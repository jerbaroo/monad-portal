{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE UndecidableInstances   #-}

module Telescope.Class (module Telescope.Class, PrimaryKey) where

import           Control.Monad            ( join )
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
    let rowsMayF = Map.lookup <$> tableKeyF <*> rowsF -- Rows for the table.
    pure $ join <$> (fmap . Map.lookup <$> rowKeyF <*> rowsMayF)

  -- | View multiple rows in a data source.
  viewRows :: f Table.RowsIndex -> m (f (Table.Tables))
  viewRows rowKeysMapF = do
    tablesF <- viewTables $ Map.keys <$> rowKeysMapF
    pure $ Map.intersectionWith onlyRows <$> tablesF <*> rowKeysMapF
    where onlyRows :: Table.Table -> [Table.RowKey] -> Table.Table
          onlyRows table = Map.restrictKeys table . Set.fromList

  -- | View one table in a data source.
  viewTable :: f Table.TableKey -> m (f Table.Table)
  viewTable tableKeyF = do
    tablesF <- viewTables $ (:[]) <$> tableKeyF
    pure $ maybe Map.empty id <$> (Map.lookup <$> tableKeyF <*> tablesF)

  -- | View multiple tables in a data source.
  viewTables :: f [Table.TableKey] -> m (f Table.Tables)

  -- | Set one row in a data source.
  setRow :: f Table.TableKey -> f Table.RowKey -> f Table.Row -> m ()
  setRow tableKeyF rowKeyF rowF =
    setRows $ Map.singleton <$> tableKeyF <*>
      (Map.singleton <$> rowKeyF <*> rowF)

  -- | Set multiple rows in a data source.
  setRows :: f Table.Tables -> m ()
  setRows newRowsMapF = do
    tablesF <- viewTables $ Map.keys <$> newRowsMapF
    setTables $ Map.unionWith Map.union <$> newRowsMapF <*> tablesF

  -- | Set one table in a data source.
  setTable :: f Table.TableKey -> f Table.Table -> m ()
  setTable tableKeyF tableF = setTables $ Map.singleton <$> tableKeyF <*> tableF

  -- | Set multiple tables in a data source.
  setTables :: f Table.Tables -> m ()

  -- | Remove one row from a data source.
  rmRow :: f Table.TableKey -> f Table.RowKey -> m ()
  rmRow tableKeyF rowKeyF =
    rmRows $ Map.singleton <$> tableKeyF <*> ((:[]) <$> rowKeyF)

  -- | Remove multiple rows from a data source.
  rmRows :: f Table.RowsIndex -> m ()
  rmRows rowKeysMapF = do
    tablesF <- viewTables $ Map.keys <$> rowKeysMapF
    setTables $ Map.intersectionWith withoutRows <$> tablesF <*> rowKeysMapF
    where withoutRows :: Table.Table -> [Table.RowKey] -> Table.Table
          withoutRows table = Map.withoutKeys table . Set.fromList

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
class    (Store.ToSDataType a k, Store.FromSValues a) => Entity a k where
instance (Store.ToSDataType a k, Store.FromSValues a) => Entity a k where
