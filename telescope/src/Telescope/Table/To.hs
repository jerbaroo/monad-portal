{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

{- | Conversion to table representation.

See "Telescope.Storable.Types" for context.
-}
module Telescope.Table.To where

import qualified Data.Map                 as Map
import           Data.Proxy                ( Proxy(..) )
import           Data.Text                 ( Text )
import           Data.Typeable             ( Typeable, typeRep )
import qualified Telescope.Storable.Types as Storable
import qualified Telescope.Table.Types    as Table

--------------------------------------------------------------------------------
-- * Conversion to Primitive Values
--
-- $conversionToPrimitiveValues

-- | A data type that can be converted to a non-null storable primitive.
class ToPrimNotNull a where
  toPrimNotNull :: a -> Table.PrimNotNull

instance ToPrimNotNull Bool where toPrimNotNull = Table.PrimBool
instance ToPrimNotNull Int  where toPrimNotNull = Table.PrimInt
instance ToPrimNotNull Text where toPrimNotNull = Table.PrimText

-- | A data type that can be converted to a storable primitive.
class ToPrim a where
  toPrim :: a -> Table.Prim

instance ToPrim Bool where toPrim = Table.PrimNotNull . toPrimNotNull
instance ToPrim Int  where toPrim = Table.PrimNotNull . toPrimNotNull
instance ToPrim Text where toPrim = Table.PrimNotNull . toPrimNotNull
instance ToPrim a => ToPrim (Maybe a) where
  toPrim Nothing  = Table.PrimNull
  toPrim (Just a) = toPrim a

--------------------------------------------------------------------------------
-- * Conversion to Table Keys
--
-- $conversionToTableKeys

-- | A 'TableKey' for a data type.
tableKey :: forall a. Typeable a => Table.TableKey
tableKey = Table.TableKey $ show $ typeRep $ Proxy @a

-- | A data type that has a primary key.
class (Eq k, ToRowKey k) => PrimaryKey a k | a -> k where
  primaryKey :: a -> k

-- | A data type that can be converted to a key.
--
-- Currently only simple data types and tuples thereof are supported.
class ToRowKey a where
  toRowKey :: a -> Table.RowKey

instance ToPrimNotNull a => ToRowKey a where
  toRowKey a = Table.RowKey (toPrimNotNull a) []
instance (ToPrimNotNull a, ToPrimNotNull b) => ToRowKey (a, b) where
  toRowKey (a, b) = Table.RowKey (toPrimNotNull a) [toPrimNotNull b]
instance (ToPrimNotNull a, ToPrimNotNull b, ToPrimNotNull c) => ToRowKey (a, b, c) where
  toRowKey (a, b, c) = Table.RowKey (toPrimNotNull a) [toPrimNotNull b, toPrimNotNull c]
instance (ToPrimNotNull a, ToPrimNotNull b, ToPrimNotNull c, ToPrimNotNull d) => ToRowKey (a, b, c, d) where
  toRowKey (a, b, c, d) = Table.RowKey (toPrimNotNull a) [toPrimNotNull b, toPrimNotNull c, toPrimNotNull d]

-- | 'RowKey' for a data type with a primary key.
rowKey :: PrimaryKey a k => a -> Table.RowKey
rowKey = toRowKey . primaryKey

--------------------------------------------------------------------------------
-- * Conversion from Storable Representation
--
-- $conversionFromStorableRepresentation

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
