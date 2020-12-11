{-# LANGUAGE DeriveGeneric         #-}

{- | Intermediate storable representation of a data type.

See "Telescope.Convert" for context. Intended for internal use only.
-}
module Telescope.Storable.Types where

import           GHC.Generics           ( Generic )
import           Telescope.Table.Types as Table


-- | A storable representation of a field's value.
data SValue =
  -- | A fields's value that is a storable primitive.
    SValuePrim Table.Prim
  -- | A field's value that is zero or more storable data types.
  | SValueDataTypes [SDataType]
  deriving (Eq, Generic, Show)

-- | A storable representation of ALL fields of a data type.
newtype SFields = SFields [(Table.ColumnKey, SValue)]
  deriving (Eq, Generic, Show)

-- | A storable representation of a data type (type and fields).
data SDataType = SDataType Table.Ref SFields
  deriving (Eq, Generic, Show)
