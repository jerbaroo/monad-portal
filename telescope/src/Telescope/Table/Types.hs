{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- The table representation of data types.
module Telescope.Table.Types where

import           Data.Aeson           ( FromJSON, ToJSON )
import           Data.Map            as Map
import           Data.Set             ( Set )
import           Data.Text            ( Text )
import           Flat                 ( Flat )
import           GHC.Generics         ( Generic )

-- | A storable primitive, a cell in a table.
data Prim =
    PBool Bool
  | PInt  Int
  | PText Text
  | PNull
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | A key consists of one or more storable primitives.
--
-- Individual keys, and composite keys are supported.
--
-- Examples:
--   "foo"             -> KeyOne (PText "foo")
--   (5 :: Int, "foo") -> KeyMore [PInt 5, PText "foo"]
data Key =
    KeyOne Prim
  | KeyMore [Prim]
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | Unique identifier for a table in a database.
--
-- Derived via 'Typeable' from the type of a data type.
-- Example: '"Person"' for 'Person { name = "John", age = 21 }'.
newtype TableKey = TableKey { unTableKey :: String }
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | Unique identifier for a row in a database table.
--
-- Corresponds to the primary key of a data type.
-- Example: '"John"' for 'Person { name = "John", age = 21 }'.
newtype RowKey = RowKey { unRowKey :: Key }
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | Unique identifier for a column in a database table.
--
-- Derived from the name of a field of a data type
-- Example: '"name"' for 'data Person { name :: String, age :: Int }'.
newtype ColumnKey = ColumnKey { unColumnKey :: String }
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | Unique identifier for a data type in a database.
type Ref = (TableKey, RowKey)

-- | A row consists of a serialized value per column.
-- TODO: move from 'String' to 'Prim'.
type Row = [(ColumnKey, Prim)]

-- | A table consists of a number of rows, each with a key.
-- TODO: consider alterate to 'Row' to avoid duplicates of 'ColumnKey'.
type Table = Map RowKey Row

-- | A number of tables indexed by 'TableKey'.
type Tables = Map TableKey Table

-- | Indices into a number of rows across tables.
type RowsIndex = Map TableKey (Set RowKey)

--------------------
-- JSON INSTANCES --
--------------------

instance FromJSON ColumnKey
instance FromJSON Key
instance FromJSON Prim
instance FromJSON RowKey

instance ToJSON ColumnKey
instance ToJSON Key
instance ToJSON Prim
instance ToJSON RowKey
