{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- The table representation of data types.
module Telescope.Table.Types where

import           Data.Aeson           ( FromJSON, ToJSON )
import           Data.Map             ( Map )
import           Data.Set             ( Set )
import           Data.Text            ( Text )
import           Flat                 ( Flat )
import           GHC.Generics         ( Generic )

-- | A non-null storable primitive. One cell in a table.
data PrimNotNull =
    PrimBool Bool
  | PrimInt  Int
  | PrimText Text
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | A storable primitive. One cell in a table.
data Prim = PrimNotNull PrimNotNull | PrimNull
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | Unique identifier for a database table.
newtype TableKey = TableKey { unTableKey :: String }
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | Unique identifier for a row in a database table.
--
-- Individual keys, and composite keys are supported.
--
-- Examples:
--   "foo"             -> RowKey (PrimText "foo") []
--   (5 :: Int, "foo") -> RowKey (PrimInt 5) [PrimText "foo"]
data RowKey = RowKey PrimNotNull [PrimNotNull]
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | Unique identifier for a column in a database table.
newtype ColumnKey = ColumnKey { unColumnKey :: String }
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | Unique identifier for a data type in a database.
type Ref = (TableKey, RowKey)

-- | A row consists of a serialized value per column.
type Row = [(ColumnKey, Prim)]

-- | A table consists of a number of rows, each with a key.
-- TODO: consider alterate to 'Row' to avoid duplicates of 'ColumnKey'.
type Table = Map RowKey Row

-- | A number of tables indexed by 'TableKey'.
type Tables = Map TableKey Table

-- | Indices into a number of rows across tables.
type RowIndices = Map TableKey (Set RowKey)

--------------------
-- JSON INSTANCES --
--------------------

instance FromJSON ColumnKey
instance FromJSON Prim
instance FromJSON PrimNotNull
instance FromJSON RowKey
instance FromJSON TableKey

instance ToJSON ColumnKey
instance ToJSON Prim
instance ToJSON PrimNotNull
instance ToJSON RowKey
instance ToJSON TableKey
