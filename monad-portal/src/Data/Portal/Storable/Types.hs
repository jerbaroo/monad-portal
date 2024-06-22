{-# LANGUAGE DeriveGeneric #-}

-- | Intermediate storable representation of a data type.
module Data.Portal.Storable.Types where

import           GHC.Generics           ( Generic )
import           Data.Portal.Table.Types as Table

-- | Storable representation of a field's value.
-- TODO: rename to SFieldValue
data SValue =
  -- | A fields's value that is a storable primitive.
    SValuePrim Table.Prim
  -- | A field's value that is zero or more storable data types.
  | SValueDataType SDataType
  deriving (Eq, Generic, Show)

type SField = (Table.ColumnKey, SValue)

-- | Storable representation of ALL fields of a data type.
newtype SFields = SFields [SField]
  deriving (Eq, Generic, Show)

-- | A storable representation of a data type (type and fields).
newtype SDataType = SDataType (Table.Ref, SFields)
  deriving (Eq, Generic, Show)
