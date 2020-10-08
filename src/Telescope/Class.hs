{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Telescope.Class (module Telescope.Class, PKey) where

import qualified Data.Key        as Key
import qualified Data.Map                      as Map
import qualified Telescope.Store               as Store
import qualified Telescope.Table               as Table
import           Telescope.Table                ( PKey )

-- | Defines primitive operations to interact with a data source.
--
-- All operations are based on 'Table.Row' and similar data types.
class (Functor m, Applicative m, Monad m) => Telescope m where
  -- | View a row in a table in a data source.
  --
  -- 'Maybe' to indicate the entry might not be found.
  view :: Table.TableKey -> Table.RowKey -> m (Maybe Table.Row)
  view tk rk = Map.lookup rk <$> viewTable tk

  -- ^ View all rows in a table in a data source.
  viewTable :: Table.TableKey -> m Table.Table
 
  -- | Set a single row in a table in a data source.
  --
  -- An existing row with the same key will be overwritten.
  -- Any other processes watching for updates will be notified.
  set :: Table.TableKey -> Table.RowKey -> Table.Row -> m ()
  set tableKey rowKey r = do
    table <- viewTable tableKey
    setTable tableKey $ Map.insert rowKey r table

  -- | Set multiple rows across multiple tables in a data source.
  setMany :: Map.Map Table.TableKey Table.Table -> m ()
  setMany many =
    -- Set each nested entity in the given entity 'a'.
    Key.forWithKeyM_ many $ \tableKey table ->
      Key.forWithKeyM_ table $ \rowKey row ->
        set tableKey rowKey row

  -- | Set a table in a data source to the given rows.
  --
  -- All existing rows in the table will be removed.
  setTable :: Table.TableKey -> Table.Table -> m ()

  -- | Remove a row in a table in a data source.
  rm :: Table.TableKey -> Table.RowKey -> m ()
  rm tableKey rowKey = do
    table <- viewTable tableKey
    setTable tableKey $ Map.delete rowKey table

  -- | Remove a table in a data source.
  rmTable :: Table.TableKey -> m ()
  rmTable tk = setTable tk Map.empty

  -- ^ Modify a row in a data source.
  -- TODO: this requires decoding to work.

-- | User-facing class for storable data types.
--
-- Serialization and deserialization under one typeclass.
class    (Store.ToSDataType a, Store.FromSValues a) => Entity a where
instance (Store.ToSDataType a, Store.FromSValues a ) => Entity a where
