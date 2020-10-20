{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE QuantifiedConstraints  #-}

module Telescope.Class (module Telescope.Class, PrimaryKey) where

import           Control.Monad     ( void )
import qualified Data.Map         as Map
import qualified Telescope.Store  as Store
import qualified Telescope.Table  as Table
import           Telescope.Table   ( PrimaryKey )

-- | A data type that can be put in a container.
class ToFromF f a where
  toF   :: a -> f a
  fromF :: f a -> a

-- | Defines primitive operations to interact with a data source.
--
-- All operations are based on 'Table.Row' and similar data types.
class (Applicative f, Monad m, forall a. ToFromF f a) => Telescope m f | m -> f where
  escape :: f a -> m a
  escape = pure . fromF
  enter  :: a -> m (f a)
  enter = pure . toF

  -- | View a table row in a data source.
  --
  -- 'Maybe' to indicate the entry might not be found.
  viewRow :: f Table.TableKey -> f Table.RowKey -> m (f (Maybe Table.Row))
  viewRow tableKey rowKey = do
    tables <- viewTableRows tableKey
    pure $ Map.lookup <$> rowKey <*> tables

  -- | View all table rows in a data source.
  viewTableRows :: f Table.TableKey -> m (f Table.Table)

  -- | Set a single table row in a data source.
  --
  -- An existing row with the same key will be overwritten.
  -- Any other processes watching for updates will be notified.
  setRow :: f Table.TableKey -> f Table.RowKey -> f Table.Row -> m ()
  setRow tableKey rowKey row = do
    rows <- viewTableRows tableKey
    setTableRows tableKey $ Map.insert <$> rowKey <*> row <*> rows

  -- | Set multiple rows across multiple tables in a data source.
  setManyRows :: f (Map.Map Table.TableKey Table.Table) -> m ()
  setManyRows many = do
    -- Convert the nested map into a flattened list.
    let toRows = (\tableMap -> [
          (tableKey, rowKey, row)
          | (tableKey, table) <- zip (Map.keys tableMap) (Map.elems tableMap)
          , (rowKey  , row  ) <- zip (Map.keys table   ) (Map.elems table   )
          ]) <$> many
    -- Sequence a list of operations to set each row.
    void $ sequence =<< (escape $
        (fmap . fmap) (\(a, b, c) -> setRow (pure a) (pure b) (pure c)) toRows)

  -- | Set a table in a data source to ONLY the given rows.
  --
  -- All existing rows in the table will be removed.
  setTableRows :: f Table.TableKey -> f Table.Table -> m ()

  -- | Remove a table row in a data source.
  rmRow :: f Table.TableKey -> f Table.RowKey -> m ()
  rmRow tableKey rowKey = do
    rows <- viewTableRows tableKey
    setTableRows tableKey $ Map.delete <$> rowKey <*> rows

  -- | Remove a table in a data source.
  rmTableRows :: f Table.TableKey -> m ()
  rmTableRows tableKey = setTableRows tableKey $ pure Map.empty

  -- | Run a function when a table row in a data source changes.
  onChangeRow ::
    f Table.TableKey -> f Table.RowKey -> f (Maybe Table.Row -> m ()) -> m ()

-- | User-facing class for storable data types.
--
-- Serialization and deserialization under one typeclass.
class    (Store.ToSDataType a, Store.FromSValues a) => Entity a where
instance (Store.ToSDataType a, Store.FromSValues a) => Entity a where
