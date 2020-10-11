{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Telescope.Class (module Telescope.Class, PrimaryKey) where

import qualified Data.Key        as Key
import qualified Data.Map                      as Map
import qualified Telescope.Store               as Store
import qualified Telescope.Table               as Table
import           Telescope.Table                ( PrimaryKey )

-- | Defines primitive operations to interact with a data source.
--
-- All operations are based on 'Table.Row' and similar data types.
class (Functor m, Applicative m, Monad m) => Telescope m where
  -- | View a row in a table in a data source.
  --
  -- 'Maybe' to indicate the entry might not be found.
  viewRow :: Table.TableKey -> Table.RowKey -> m (Maybe Table.Row)
  viewRow tableKey rowKey = Map.lookup rowKey <$> viewTableRows tableKey

  -- ^ View all rows in a table in a data source.
  viewTableRows :: Table.TableKey -> m Table.Table
 
  -- | Set a single row in a table in a data source.
  --
  -- An existing row with the same key will be overwritten.
  -- Any other processes watching for updates will be notified.
  setRow :: Table.TableKey -> Table.RowKey -> Table.Row -> m ()
  setRow tableKey rowKey r = do
    rows <- viewTableRows tableKey
    setTableRows tableKey $ Map.insert rowKey r rows

  -- | Set multiple rows across multiple tables in a data source.
  setManyRows :: Map.Map Table.TableKey Table.Table -> m ()
  setManyRows many =
    -- Set each nested entity in the given entity 'a'.
    Key.forWithKeyM_ many $ \tableKey table ->
      Key.forWithKeyM_ table $ \rowKey row ->
        setRow tableKey rowKey row

  -- | Set a table in a data source to ONLY the given rows.
  --
  -- All existing rows in the table will be removed.
  setTableRows :: Table.TableKey -> Table.Table -> m ()

  -- | Remove a row in a table in a data source.
  rmRow :: Table.TableKey -> Table.RowKey -> m ()
  rmRow tableKey rowKey = do
    rows <- viewTableRows tableKey
    setTableRows tableKey $ Map.delete rowKey rows

  -- | Remove a table in a data source.
  rmTableRows :: Table.TableKey -> m ()
  rmTableRows tableKey = setTableRows tableKey Map.empty

  -- | Run a function when a row in a data source changes.
  onChangeRow :: Table.TableKey -> Table.RowKey -> (Maybe Table.Row -> m ()) -> m ()

-- | User-facing class for storable data types.
--
-- Serialization and deserialization under one typeclass.
class    (Store.ToSDataType a, Store.FromSValues a) => Entity a where
instance (Store.ToSDataType a, Store.FromSValues a) => Entity a where
