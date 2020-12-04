{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- The table representation of data types.
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

-- * JSON Instances
--
-- $JSONInstances

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

--------------------------------------------------------------------------------
-- Conversion of lists to/from table representation ----------------------------
--------------------------------------------------------------------------------

-- * Lists of Primitives
--
-- $listsOfPrimitives
--
-- In the 'Telescope.Storable' modules, lists of type 'ToPrim a => [a]' can
-- themselves be converted to a storable primitive value with 'Text' as the
-- underlying primitive type. An instance is provided for these lists which
-- first converts each list element to a 'Prim'. One solution would be to call
-- 'show' on each 'Prim', however this would result in converting 'True' to
-- '"PrimNotNull (PrimBool True)"', and similarly for every element, resulting
-- in a lot of redundant information.
--
-- To remedy this a function is defined here, 'primShow', which will convert
-- 'True' to '"BTrue"'. 'primRead' is also defined for the reverse operation.

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
