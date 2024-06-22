{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Conversion to storable representation.
module Data.Portal.Storable.To where

import           Control.Exception         ( throw )
import           Data.Typeable             ( Typeable )
import           Data.Proxy                ( Proxy(Proxy) )
import           Data.Text                 ( Text, pack )
import qualified Generics.Eot             as Eot
import qualified Data.Portal.Exception      as E
import           Data.Portal.Storable.Types  ( SDataType(..), SFields(..),
                                             SValue(..) )
import qualified Data.Portal.Table.To       as Table
import qualified Data.Portal.Table.Types    as Table

-- | A field's value that can be converted to storable representation.
class ToSValue a where
  toSValue :: a -> SValue

instance ToSValue Bool where toSValue = SValuePrim . Table.toPrim
instance ToSValue Int  where toSValue = SValuePrim . Table.toPrim
instance ToSValue Text where toSValue = SValuePrim . Table.toPrim

instance ToSValue a => ToSValue (Maybe a) where
  toSValue Nothing = SValuePrim Table.PrimNull
  toSValue (Just a) = toSValue a

-- | A list of storable primitives is stored as 'Table.PrimText'.
instance Table.ToPrim a => ToSValue [a] where
  toSValue t = SValuePrim $ Table.PrimNotNull $
    Table.PrimText $ pack $ show $ (Table.primShow . Table.toPrim) <$> t

instance {-# OVERLAPPABLE #-} ToSDataType a k => ToSValue a where
  toSValue = SValueDataType . toSDataType

-- | Types where ALL field's values can be converted to storable representation.
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
-- that a data type has more than one constructor. MonadPortal does not support
-- data types with multiple constructors.
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
  case Eot.constructors $ Eot.datatype $ Proxy @a of
    []  -> throw E.NoConstructorsException
    [c] -> case Eot.fields c of
      Eot.Selectors   fns -> fns
      Eot.NoSelectors _   -> throw E.NoSelectorsException
      Eot.NoFields        -> throw E.NoFieldsException
    _ -> throw E.MultipleConstructorsException

-- | Types that can be converted to storable representation.
type ToSDataType a k = (Typeable a, Table.HasRowKey a, ToSFields a)

-- | Convert to storable representation.
toSDataType :: forall a k. ToSDataType a k => a -> SDataType
toSDataType a = SDataType ((Table.tableKey @a, Table.rowKey a), (toSFields a))
