{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

-- Conversion to table representation.
module Telescope.Table.To where

import qualified Data.Map                 as Map
import           Data.Proxy                ( Proxy(..) )
import           Data.Text                 ( Text )
import           Data.Typeable             ( Typeable, typeRep )
import qualified Telescope.Storable.Types as Storable
import qualified Telescope.Table.Types    as Table

--------------------------------------------------------------------------------
-- Primitives ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | A data type that can be converted to a storable primitive.
class ToPrim a where
  toPrim :: a -> Table.Prim

instance ToPrim Bool where toPrim = Table.PBool
instance ToPrim Int  where toPrim = Table.PInt
instance ToPrim Text where toPrim = Table.PText
instance ToPrim a => ToPrim (Maybe a) where
  toPrim Nothing  = Table.PNull
  toPrim (Just a) = toPrim a

-- | A data type that can be converted to a key.
--
-- Currently only simple data types and tuples thereof are supported.
class ToKey a where
  toKey :: a -> Table.Key

instance ToPrim a => ToKey a where
  toKey a = Table.KeyOne $ toPrim a
instance (ToPrim a, ToPrim b) => ToKey (a, b) where
  toKey (a, b) = Table.KeyMore [toPrim a, toPrim b]
instance (ToPrim a, ToPrim b, ToPrim c) => ToKey (a, b, c) where
  toKey (a, b, c) = Table.KeyMore [toPrim a, toPrim b, toPrim c]
instance (ToPrim a, ToPrim b, ToPrim c, ToPrim d) => ToKey (a, b, c, d) where
  toKey (a, b, c, d) = Table.KeyMore [toPrim a, toPrim b, toPrim c, toPrim d]

--------------------------------------------------------------------------------
-- Key types -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | A 'TableKey' for a data type.
tableKey :: forall a. Typeable a => Table.TableKey
tableKey = Table.TableKey $ show $ typeRep $ Proxy @a

-- | A data type that has a primary key.
class (Eq k, ToKey k) => PrimaryKey a k | a -> k where
  primaryKey :: a -> k

-- | 'RowKey' for a data type with a primary key.
rowKey :: PrimaryKey a k => a -> Table.RowKey
rowKey = Table.RowKey . toKey . primaryKey

--------------------------------------------------------------------------------
-- From storable representation ------------------------------------------------
--------------------------------------------------------------------------------

-- | Convert from storable to table representation.
sToRows :: Storable.SDataType -> Table.Tables
sToRows a = Map.fromList [(tk, table)]
  where (Storable.SDataType ref fields) = a
        (tk, rk)                        = ref
        table                           =
          Map.fromList [(rk, sFieldsToRow fields)]

-- | Convert 'SFields' to a table row.
--
-- TODO: https://github.com/jerbaroo/new-telescope/issues/17
-- This conversion only handles 'SValue's that are primitives.
-- Converstion of nested data types does not yet work.
sFieldsToRow :: Storable.SFields -> Table.Row
sFieldsToRow (Storable.SFields fieldsMap) =
  [ (columnKey, fromSValue sValue)
  | (columnKey, sValue) <- fieldsMap
  ]
  where fromSValue (Storable.SValuePrim prim) = prim
