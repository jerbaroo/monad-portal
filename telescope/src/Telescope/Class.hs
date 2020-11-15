{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | The important user-facing type classes.
module Telescope.Class (module Telescope.Class, Table.PrimaryKey(..)) where

import           Control.Monad            ( join )
import qualified Data.Map                as Map
import           Data.Set                 ( Set )
import qualified Data.Set                as Set
import           Telescope.Storable.From  ( FromSValues )
import           Telescope.Storable.To    ( ToSDataType )
import           Telescope.Table.To      as Table
import           Telescope.Table.Types   as Table

-- | A storable datatype (can be serialized and deserialized via Generics).
type Entity a k = (ToSDataType a k, FromSValues a)

-- | 'Table.Row'-based operations for interacting with a data source.
class (Applicative f, Monad m) => Telescope m f | m -> f where

  -- | View one row in a data source.
  viewRow :: f Table.TableKey -> f Table.RowKey -> m (f (Maybe Table.Row))
  viewRow tableKeyF rowKeyF = do
    rowsF <- viewRows $
      Map.singleton <$> tableKeyF <*> (Set.singleton <$> rowKeyF)
    let rowsMayF = Map.lookup <$> tableKeyF <*> rowsF -- Rows for the table.
    pure $ join <$> (fmap . Map.lookup <$> rowKeyF <*> rowsMayF)

  -- | View multiple rows in a data source.
  viewRows :: f Table.RowsIndex -> m (f (Table.Tables))
  viewRows rowKeysMapF = do
    tablesF <- viewTables $ Set.fromList . Map.keys <$> rowKeysMapF
    pure $ Map.intersectionWith Map.restrictKeys <$> tablesF <*> rowKeysMapF

  -- | View one table in a data source.
  viewTable :: f Table.TableKey -> m (f Table.Table)
  viewTable tableKeyF = do
    tablesF <- viewTables $ Set.singleton <$> tableKeyF
    pure $ maybe Map.empty id <$> (Map.lookup <$> tableKeyF <*> tablesF)

  -- | View multiple tables in a data source.
  viewTables :: f (Set Table.TableKey) -> m (f Table.Tables)

  -- | Set one row in a data source.
  setRow :: f Table.TableKey -> f Table.RowKey -> f Table.Row -> m ()
  setRow tableKeyF rowKeyF rowF =
    setRows $ Map.singleton <$> tableKeyF <*>
      (Map.singleton <$> rowKeyF <*> rowF)

  -- | Set multiple rows in a data source.
  setRows :: f Table.Tables -> m ()
  setRows newRowsMapF = do
    tablesF <- viewTables $ Set.fromList . Map.keys <$> newRowsMapF
    setTables $ Map.unionWith Map.union <$> newRowsMapF <*> tablesF

  -- | Set one table in a data source.
  setTable :: f Table.TableKey -> f Table.Table -> m ()
  setTable tableKeyF tableF = setTables $ Map.singleton <$> tableKeyF <*> tableF

  -- | Set multiple tables in a data source.
  setTables :: f Table.Tables -> m ()

  -- | Remove one row from a data source.
  rmRow :: f Table.TableKey -> f Table.RowKey -> m ()
  rmRow tableKeyF rowKeyF =
    rmRows $ Map.singleton <$> tableKeyF <*> (Set.singleton <$> rowKeyF)

  -- | Remove multiple rows from a data source.
  rmRows :: f Table.RowsIndex -> m ()
  rmRows rowKeysMapF = do
    tablesF <- viewTables $ Set.fromList . Map.keys <$> rowKeysMapF
    setTables $ Map.intersectionWith Map.withoutKeys <$> tablesF <*> rowKeysMapF

  -- | Remove one table from a data source.
  rmTable :: f Table.TableKey -> m ()
  rmTable tableKeyF = rmTables $ Set.singleton <$> tableKeyF

  -- | Remove multiples table from a data source.
  rmTables :: f (Set Table.TableKey) -> m ()
  rmTables tableKeysF = setTables $ Map.fromSet (const Map.empty) <$> tableKeysF

  -- | Run a function when a row in a data source changes.
  onChangeRow
      :: f Table.TableKey
      -> f Table.RowKey
      -> f (Maybe Table.Row -> m ())
      -> m ()

  perform :: f (m ()) -> m ()
