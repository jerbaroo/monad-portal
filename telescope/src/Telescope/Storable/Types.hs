{-# LANGUAGE DeriveGeneric         #-}

-- Intermediate storable representation of a data type.
module Telescope.Storable.Types where

import           GHC.Generics           ( Generic )
import           Telescope.Table.Types as Table

-- This module defines an intermediate storable representation of a data type.
-- This representation is converted to table representation by the 'Telescope'
-- typeclass when writing a data type to a data source, and reconstructed from
-- table representation when reading a data type from a data source.
--
--   data type <--> storable representation <--> table representation
--
-- The main difference between the storable and table representations is that in
-- the table representation everything is flattened into rows of primitives. In
-- the storable representation, a data type consists of the same primitives, but
-- the data type is not yet flattened into rows. See 'Telescope.Table' for a
-- description of the primitives used in table and storable representations.
--
-- Some terminology, consider the following data type:
--
--   data Person { name :: Text }
--   somePerson = Person { name = "John" }
--
--   Here we refer to:
--     - "Person" as the type.
--     - "name" as the field's name.
--     - "John" as the field's value.

-- | A storable representation of a field's value.
data SValue =
  -- A fields's value that is a storable primitive.
    SValuePrim Table.Prim
  -- A field's value that is zero or more storable data types.
  | SValueDataTypes [SDataType]
  deriving (Eq, Generic, Show)

-- | A storable representation of ALL fields of a data type.
newtype SFields = SFields [(Table.ColumnKey, SValue)]
  deriving (Eq, Generic, Show)

-- | A storable representation of a data type (type and fields).
data SDataType = SDataType Table.Ref SFields
  deriving (Eq, Generic, Show)
