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

-- | A non-null storable primitive. One cell in a table.
data PrimNotNull =
    PrimBool Bool
  | PrimInt  Int
  | PrimText Text
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | A storable primitive, or not. One cell in a table.
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

-- | Unique identifier for a database column.
newtype ColumnKey = ColumnKey Key
  deriving (Eq, Ord, Read, Show, Generic, Flat)

-- | Unique identifier for a database cell.
type Ref = (TableKey, RowKey)

-- | A row consists of a serialized value per column.
type Row = [(ColumnKey, Prim)]

-- | A table consists of a number of rows, each with a key.
-- TODO: consider alterate to 'Row' to avoid duplicates of 'ColumnKey'.
type Table = Map RowKey Row

-- | Database tables. Each table indexed by 'TableKey'.
type Tables = Map TableKey Table

-- | Database rows. Possibly spanning multiple tables.
type Rows = Map TableKey (Set RowKey)

-- | More efficient conversion to human-readable 'String' than 'show'.
primShow :: Prim -> String
primShow (PrimNotNull (PrimBool b)) = "B" ++ show b
primShow (PrimNotNull (PrimInt  i)) = "I" ++ show i
primShow (PrimNotNull (PrimText t)) = "T" ++ show t
primShow PrimNull                   = "N"

-- | More efficient conversion from human-readable 'String' than 'read'.
primRead :: String -> Prim
primRead ('B':s) = PrimNotNull $ PrimBool $ read s
primRead ('I':s) = PrimNotNull $ PrimInt  $ read s
primRead ('T':s) = PrimNotNull $ PrimText $ read s
primRead "N"     = PrimNull
primRead s       = throw $ E.DeserializeException $
  "Unknown string in 'primRead': " ++ show s
