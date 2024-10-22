-- | Convert data types to and from table representation.
--
-- Default implementations convert via storable representation.
module Data.Portal.Table.Class where

import Data.Portal.Storable.To qualified as Storable
import Data.Portal.Storable.From qualified as Storable
import Data.Portal.Table.To qualified as Table
import Data.Portal.Table.From qualified as Table
import Data.Portal.Table.Types qualified as Table

class FromTable a where
  -- | Convert from table representation.
  fromRows :: Table.Row -> a

-- | Default implementation via storable representation.
instance {-# OVERLAPPABLE #-} Storable.FromSValues a => FromTable a where
  fromRows = Storable.fromSValues . Table.rowToSValues

class ToTable a where
  -- | Convert to table representation.
  toRows :: a -> Table.Rows

-- | Default implementation via storable representation.
instance {-# OVERLAPPABLE #-} Storable.ToSDataType a k => ToTable a where
  toRows = Table.sToRows . Storable.toSDataType
