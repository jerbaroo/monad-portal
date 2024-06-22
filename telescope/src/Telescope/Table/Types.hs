{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | Table representation.
module Telescope.Table.Types where

import           Control.Exception    ( throw )
import           Data.Aeson           ( FromJSON, ToJSON )
import           Data.Map             ( Map )
import           Data.Set             ( Set )
import           Data.Text            ( Text )
import           Flat                 ( Flat )
import           GHC.Generics         ( Generic )
import qualified Telescope.Exception as E

-- | A non-null storable primitive.
data PrimNotNull =
    PrimBool Bool
  | PrimInt  Int -- TODO Int64
  | PrimRef  Ref
  | PrimText Text
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | A storable primitive, or null.
data Prim = PrimNotNull PrimNotNull | PrimNull
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

-- | More efficient conversion of 'Prim' to 'String' than 'show'.
primShow :: Prim -> String
primShow (PrimNotNull (PrimBool True )) = "T"
primShow (PrimNotNull (PrimBool False)) = "F"
primShow (PrimNotNull (PrimInt  i    )) = "I" <> show i
primShow (PrimNotNull (PrimText t    )) = "S" <> show t
primShow (PrimNotNull (PrimRef r     )) = "R" <> show r
primShow PrimNull                       = "N"

-- | More efficient conversion of 'Prim' from 'String' than 'read'.
primRead :: String -> Prim
primRead "T"     = PrimNotNull $ PrimBool True
primRead "F"     = PrimNotNull $ PrimBool False
primRead ('I':s) = PrimNotNull $ PrimInt  $ read s
primRead ('R':s) = PrimNotNull $ PrimRef $ read s
primRead ('S':s) = PrimNotNull $ PrimText $ read s
primRead "N"     = PrimNull
primRead s       = throw $ E.DeserializeException $
  "Unknown string in 'primRead': " <> show s
