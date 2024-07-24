-- | Table representation.
module Data.Portal.Table.Types where

import Control.Exception ( throw )
import Data.Aeson ( FromJSON, ToJSON )
import Data.Map ( Map )
import Data.Int ( Int64 )
import Data.Set ( Set )
import Data.Text ( Text )
import Flat ( Flat )
import GHC.Generics ( Generic )
import Data.Portal.Exception qualified as E

-- | A non-null storable primitive.
data PrimNotNull =
    PrimBool Bool
  | PrimInt  Int64
  | PrimText Text
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | A storable primitive, or null.
data Prim = PrimNotNull PrimNotNull | PrimNull | PrimRef Ref
  deriving (Eq, Ord, Read, Show, Generic, Flat)

type Key = String

-- | Unique identifier for a database table.
newtype TableKey = TableKey Key
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | Unique identifier for a database row.
--
-- Individual and composite keys are supported.
--
-- Examples:
--   "foo"             -> RowKey (PrimText "foo") []
--   (5 :: Int, "foo") -> RowKey (PrimInt 5) [PrimText "foo"]
-- TODO: switch to NonEmpty.
data RowKey = RowKey PrimNotNull [PrimNotNull]
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | Unique identifier for a database cell.
type Ref = (TableKey, RowKey)

-- | Unique identifier for a database column.
newtype ColumnKey = ColumnKey Key
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | A row consists of a serialized value per column.
type Row = [(ColumnKey, Prim)]

-- | A table consists of a number of rows, each with a key.
type Table = Map RowKey Row

-- | Database rows. May span multiple tables.
type Rows = Map TableKey Table

-- | Indices to database rows. May span multiple tables.
type RowKeys = Map TableKey (Set RowKey)
