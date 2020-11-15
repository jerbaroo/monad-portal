{-# LANGUAGE MonoLocalBinds #-}

-- Convert data types to and from table representation.
module Telescope.Convert where

import qualified Telescope.Storable.To   as Storable
import qualified Telescope.Storable.From as Storable
import qualified Telescope.Table.To      as Table
import qualified Telescope.Table.From    as Table
import qualified Telescope.Table.Types   as Table

-- | Convert a data type to table representation.
aToRows :: Storable.ToSDataType a k => a -> Table.Tables
aToRows = Table.sToRows . Storable.toSDataType

-- | A data type reconstructed from table representation.
aFromRow :: Storable.FromSValues a => Table.Row -> a
aFromRow = Storable.fromSValues . Table.rowToSValues
