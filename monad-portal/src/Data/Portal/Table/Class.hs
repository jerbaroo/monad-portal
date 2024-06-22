{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Convert data types to and from table representation.
--
-- Default implementations convert via storable representation.
module Data.Portal.Table.Class where

import qualified Data.Portal.Storable.To   as Storable
import qualified Data.Portal.Storable.From as Storable
import qualified Data.Portal.Table.To      as Table
import qualified Data.Portal.Table.From    as Table
import qualified Data.Portal.Table.Types   as Table

class FromTable a where
  -- | Convert from table representation.
  fromRows :: Table.Row -> a

-- Default implementation via storable representation.
instance {-# OVERLAPPABLE #-} Storable.FromSValues a => FromTable a where
  fromRows = Storable.fromSValues . Table.rowToSValues

class ToTable a where
  -- | Convert to table representation.
  toRows :: a -> Table.Rows

-- Default implementation via storable representation.
instance {-# OVERLAPPABLE #-} Storable.ToSDataType a k => ToTable a where
  toRows = Table.sToRows . Storable.toSDataType
