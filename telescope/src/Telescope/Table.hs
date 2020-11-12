{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}

-- Definitions for tables and keys in a data source.
-- Telescope end-users should not need this module.

module Telescope.Table where

import           Data.Aeson                     ( FromJSON, ToJSON )
import           Data.Map                      as Map
import           Data.Proxy                     ( Proxy(..) )
import           Data.Serialize                 ( Serialize )
import qualified Data.Typeable                 as Data
import           GHC.Generics                   ( Generic )

-- | A storable primitive, a cell in a table.
data Prim =
    PBool   Bool
  | PInt    Int
  | PString String
  | PNull
  deriving (Eq, Ord, Read, Show, Generic, Serialize)

-- | A data type that can be converted to a storable primitive.
class ToPrim a where
  toPrim :: a -> Prim

instance ToPrim Int    where toPrim = PInt
instance ToPrim String where toPrim = PString
instance ToPrim a => ToPrim (Maybe a) where
  toPrim Nothing  = PNull
  toPrim (Just a) = toPrim a

-- | A key consists of one or more storable primitives.
--
-- Individual keys, and composite keys are supported.
--
-- Examples:
--   "foo"             -> KeyOne (PString "foo")
--   (5 :: Int, "foo") -> KeyMore [PInt 5, PString "foo"]
data Key =
    KeyOne Prim
  | KeyMore [Prim]
  deriving (Eq, Ord, Read, Show, Generic, Serialize)

-- | A data type that can be converted to a key.
--
-- Currently only simple data types and tuples thereof are supported.
class ToKey a where
  toKey :: a -> Key

instance ToPrim a => ToKey a where
  toKey a = KeyOne $ toPrim a
instance (ToPrim a, ToPrim b) => ToKey (a, b) where
  toKey (a, b) = KeyMore [toPrim a, toPrim b]
instance (ToPrim a, ToPrim b, ToPrim c) => ToKey (a, b, c) where
  toKey (a, b, c) = KeyMore [toPrim a, toPrim b, toPrim c]
instance (ToPrim a, ToPrim b, ToPrim c, ToPrim d) => ToKey (a, b, c, d) where
  toKey (a, b, c, d) = KeyMore [toPrim a, toPrim b, toPrim c, toPrim d]

-- | Unique identifier for a table in a database.
--
-- Derived via 'Typeable' from the type of a data type.
newtype TableKey  = TableKey { unTableKey :: String }
  deriving (Eq, Ord, Read, Show, Generic, Serialize)

-- | Unique identifier for a row in a database.
--
-- Corresponds to the primary key of a data type.
newtype RowKey    = RowKey    Key    deriving (Eq, Ord, Read, Show, Generic, Serialize)

-- | Unique identifier for a column in a database.
--
-- Derived from the name of a field of a data type
-- e.g. "name" in "data Person {name :: String}".
newtype ColumnKey = ColumnKey String deriving (Eq, Ord, Read, Show, Generic, Serialize)

-- | A unique identifier for a data type in a database.
type Ref = (TableKey, RowKey)

-- | A row consists of a serialized value per column.
type Row = [(ColumnKey, String)]

-- | A table consists of a number of rows, each with a key.
type Table = Map RowKey Row

-- | A number of tables indexed by 'TableKey'.
type Tables = Map TableKey Table

-- | Indexed into a number of rows across tables.
type RowsIndex = Map TableKey [RowKey]

-- | A 'TableKey' for a data type.
tableKey :: forall a. Data.Typeable a => TableKey
tableKey = TableKey $ show $ Data.typeRep $ Proxy @a

-- | A data type that has a primary key.
class (Eq k, ToKey k) => PrimaryKey a k | a -> k where
  primaryKey :: a -> k

rowKey :: PrimaryKey a k => a -> RowKey
rowKey = RowKey . toKey . primaryKey

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
