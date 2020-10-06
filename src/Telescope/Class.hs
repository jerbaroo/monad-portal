{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Telescope.Class (module Telescope.Class, PKey) where

import qualified Data.Map                      as Map
import qualified Telescope.Store               as Store
import qualified Telescope.Table               as Table
import           Telescope.Table                ( PKey )

-- | Defines primitive operations to interact with a data source.
--
-- All operations are based on 'Table.Row' and similar data types.
class (Functor m, Applicative m, Monad m) => Telescope m where
  -- ^ View a row in a data source.
  --
  -- 'Maybe' to indicate the entry might not be found.
  viewTele :: Table.TableKey -> Table.RowKey -> m (Maybe Table.Row)
  viewTele tk rk = Map.lookup rk <$> viewTeleTable tk
 
  -- ^ Set a single row in a data source.
  --
  -- An existing row with the same key will be overwritten.
  -- Any other processes watching for updates will be notified.
  setTele :: Table.TableKey -> Table.RowKey -> Table.Row -> m ()
  setTele tk rk r = do
    table <- viewTeleTable tk
    setTeleTable tk $ Map.insert rk r table

  -- ^ View all rows from a table in a data source.
  viewTeleTable :: Table.TableKey -> m Table.Table

  -- ^ Set a table in a data source to the given rows.
  --
  -- All existing rows in the table will be removed.
  setTeleTable :: Table.TableKey -> Table.Table -> m ()

  -- ^ Remove a table in a data source.
  rmTeleTable :: Table.TableKey -> m ()
  rmTeleTable tk = setTeleTable tk Map.empty

-- | User-facing class for storable data types.
class    Store.ToSDataType a => Entity a where
instance Store.ToSDataType a => Entity a where
