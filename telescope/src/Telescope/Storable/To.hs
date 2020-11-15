{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- Conversion of data types to storable representation via 'Generics.Eot'.
module Telescope.Storable.To where

import           Control.Exception         ( throw )
import           Data.Typeable             ( Typeable )
import           Data.Proxy                ( Proxy(Proxy) )
import           Data.Text                 ( Text, pack )
import qualified Generics.Eot             as Eot
import qualified Telescope.Exception      as E
import           Telescope.Storable.Types  ( SDataType(..), SFields(..),
                                             SValue(..) )
import qualified Telescope.Table.To       as Table
import qualified Telescope.Table.Types    as Table

-- This module provides functions and type classes for the conversion of data
-- types to storable representation. This conversion is accomplished via the
-- 'Generics.Eot' library. See 'Telescope.Storable' for a description of the
-- storable representation used by the Telescope framework.

--------------------------------------------------------------------------------
-- ToSValue --------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Conversion of a field's value to storable representation. -------------------
--------------------------------------------------------------------------------

-- | A field's value that can be converted to storable representation.
class ToSValue a where
  toSValue :: a -> SValue

-- Conversion of primitives:

instance ToSValue Bool where toSValue = SValuePrim . Table.PBool
instance ToSValue Int  where toSValue = SValuePrim . Table.PInt
instance ToSValue Text where toSValue = SValuePrim . Table.PText
instance Table.ToPrim a => ToSValue (Maybe a) where
  toSValue Nothing  = SValuePrim Table.PNull
  toSValue (Just a) = SValuePrim $ Table.toPrim a

-- | A field's value that is a list of primitives.
instance Table.ToPrim a => ToSValue [a] where
  toSValue t = SValuePrim $ Table.PText $ pack $ show $ fmap Table.toPrim $ t

-- | A field's value that is one or more storable data types.
instance {-# OVERLAPPABLE #-} ToSDataTypes a => ToSValue a where
  toSValue = SValueDataTypes . toSDataTypes

-- | A field's value that can be converted to zero or more storable data types.
--
-- Ideally we would not need this type class. This type class is only used in
-- an instance of the 'ToSValue' type class above. Preferably that instance
-- would be written directly, instead of going through this type class. Alas I
-- have not figured out how to accomplish that because of overlapping instances.
class ToSDataTypes a where
  toSDataTypes :: a -> [SDataType]

-- | A fields's value that can be converted to one storable data type.
instance ToSDataType a k => ToSDataTypes a where
  toSDataTypes a = [toSDataType a]

-- | A fields's value that can be converted to multiple storable data types.
instance {-# OVERLAPPABLE #-} ToSDataType a k => ToSDataTypes [a] where
  toSDataTypes = map toSDataType

--------------------------------------------------------------------------------
-- ToSValues -------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Conversion of all a data type's field's values to storable representation. --
--------------------------------------------------------------------------------

-- | A data type with all field's values convertable to storable representation.
class ToSValues a where
  toSValues :: a -> [SValue]

-- | Derive an instance for 'ToSValues' via 'Generics.Eot'.
instance (Eot.HasEot a, EotSValues (Eot.Eot a)) => ToSValues a where
  toSValues = eotSValues . Eot.toEot

-- | 'Generics.Eot' implementation of 'ToSValues'.
class EotSValues eot where
  eotSValues :: eot -> [SValue]

-- | Convert the first data constructor 'ToSValues'.
--
-- Only operate on the first constructor. Error in case of a 'Right' which mean
-- that a data type has more than one constructor. Telescope does not currently
-- support data types with multiple constructors.
instance (EotSValues l, EotSValues r) => EotSValues (Either l r) where
  eotSValues (Left  fields) = eotSValues fields
  eotSValues (Right _     ) = throw E.MultipleConstructorsException

-- | Convert the fields of an 'Eot' into 'SValue's.
--
-- Each left value of the 2-tuple represents one field of a data type. The
-- 2-tuples are right-nested, so the remaining fields are nested in 'fs'.
instance (ToSValue f, EotSValues fs) => EotSValues (f, fs) where
  eotSValues (f, fs) = toSValue f : eotSValues fs

-- | Marks the end of the fields encoded in 2-tuples.
instance EotSValues () where
  eotSValues () = []

-- | Necessary boilerplate.
instance EotSValues Eot.Void where
  eotSValues = Eot.absurd

--------------------------------------------------------------------------------
-- ToSFields -------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Conversion of all a data type's fields to storable representation. ----------
--------------------------------------------------------------------------------

-- | A data type with all field's convertable to storable representation.
class ToSFields a where
  toSFields :: a -> SFields

-- | Derive an instance for 'ToSFields' via 'Generics.Eot'.
instance (Eot.HasEot a, EotSValues (Eot.Eot a)) => ToSFields a where
  toSFields a =
    let names  = fieldNames a
        values = toSValues a
    in  SFields $ zipWith (\n v -> (Table.ColumnKey n, v)) names values

-- | The names of all a data type's fields.
fieldNames :: forall a. Eot.HasEot a => a -> [String]
fieldNames _ = do
  case Eot.constructors $ Eot.datatype $ (Proxy @a) of
    []  -> throw E.NoConstructorsException
    [c] -> case Eot.fields c of
      Eot.Selectors   fns -> fns
      Eot.NoSelectors _   -> throw E.NoSelectorsException
      Eot.NoFields        -> throw E.NoFieldsException
    _ -> throw E.MultipleConstructorsException

--------------------------------------------------------------------------------
-- ToSDataType -----------------------------------------------------------------
--------------------------------------------------------------------------------
-- Conversion of a data type to storable representation. -----------------------
--------------------------------------------------------------------------------

-- | A data type that can be converted to storable representation.
type ToSDataType a k = (Typeable a, Table.PrimaryKey a k, ToSFields a)

-- | Convert a data type to storable representation.
toSDataType :: forall a k. ToSDataType a k => a -> SDataType
toSDataType a = SDataType (Table.tableKey @a, Table.rowKey a) (toSFields a)
