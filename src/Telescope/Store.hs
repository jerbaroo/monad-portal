{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE UndecidableInstances       #-}

module Telescope.Store where

import           Control.Exception              ( Exception, throw )
import           Data.ByteString                ( ByteString )
import qualified Data.Foldable                 as Foldable
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Proxy                     ( Proxy(Proxy) )
import qualified Data.Serialize                as Serialize
import qualified Generics.Eot                  as Eot
import           GHC.Generics                   ( Generic )
import qualified Telescope.Table               as Table

-- | An error that occurs when serializing a data type.
-- TODO: move to Errors module.
data SerializeError =
    MultipleConstructorsError
  | NoConstructorsError
  | NoFieldsError
  | NoSelectorsError
  deriving Show

instance Exception SerializeError

-- * A storable representation of a data type and fields.
--
-- data Person { name :: String }
-- john = Person { name = "John" }
--
-- For some clarity on how we describe this data type:
--   'Person' is the type.
--   '"name"' is the name of a field.
--   '"John"' is the value of a field.

-- | A storable representation of a field's value.
-- TODO: consider renaming.
data SValue =
  -- ^ A primitive storable value.
  SValue Table.Prim
  -- ^ One nested storable data type.
  | SDT SDataType
  -- ^ A nested list (from any 'Foldable') of storable data types.
  | SDTs [SDataType]
  deriving (Generic, Show)

-- | A storable representation of ALL fields of a data type.
newtype SFields = SFields (Map Table.ColumnKey SValue)
  deriving (Generic, Show)

-- | A storable representation of a data type (type and fields).
data SDataType = SDataType Table.Ref SFields
  deriving (Generic, Show)

-- * Typeclasses and instances for converting to storable representation.

-- | Convert a field's value to a storable representation.
class ToSValue a where
  toSValue :: a -> SValue

-- | Convert 'Int' primitive to storable representation.
instance ToSValue Int where
  toSValue a = SValue $ Table.PInt a

-- | Convert 'String' primitive to storable representation.
instance ToSValue String where
  toSValue a = SValue $ Table.PString a

-- | Convert 'Maybe' to storable representation.
instance Table.ToPrim a => ToSValue (Maybe a) where
  toSValue Nothing  = SValue Table.PNull
  toSValue (Just a) = SValue $ Table.toPrim a

-- | A storable data type may be a field's value in another storable data type.
instance {-# OVERLAPPABLE #-} ToSDataType a => ToSValue a where
  toSValue a = SDT $ toSDataType a

-- | A list (or 'Foldable') of storable data types may be a field's value in
-- another storable data type.
instance {-# OVERLAPPABLE #-} (Foldable t, ToSDataType a)
  => ToSValue (t a) where
  toSValue t = SDTs $ map toSDataType $ Foldable.toList t

-- | Convert ALL a data type's field's values to storable representation.
class ToSValues a where
  toSValues :: a -> [SValue]

-- | Derive an instance for 'ToSValues' via Generics-Eot, implementation below.
instance {-# OVERLAPPABLE #-} (Eot.HasEot a, EotSValues (Eot.Eot a))
  => ToSValues a where
  toSValues = eotSValues . Eot.toEot

-- | Convert a data types field's to storable representation.
class ToSFields a where
  toSFields :: a -> SFields

-- | Derive an instance for 'ToSFields' via Generics.
instance {-# OVERLAPPABLE #-} (Eot.HasEot a, EotSValues (Eot.Eot a))
  => ToSFields a where
  toSFields a =
    let names  = fieldNames a
        values = toSValues a
    in  SFields $ Map.fromList $
          zipWith (\n v -> (Table.ColumnKey n, v)) names values

-- | Convert a data type to a storable representation.
--
-- TODO: 'toSDataType' may not be needed.
class (Table.HasTableKey a, Table.HasRowKey a, ToSFields a)
  => ToSDataType a where
  toSDataType :: a -> SDataType
  toSDataType a =
    SDataType (Table.tableKey a, Table.rowKey a) (toSFields a)

-- | If a data type has a table key, row key, and fields with a storable
-- representation, then it also has a storable representation.
instance {-# OVERLAPPABLE #-} (Table.HasTableKey a, Table.HasRowKey a, ToSFields a)
  => ToSDataType a where

-- * Generics-Eot implementations of 'EotSValues' and 'fieldNames'.

class EotSValues eot where
  eotSValues :: eot -> [SValue]

-- | Error in case of a 'Right', only operate on the first constructor.
instance (EotSValues a, EotSValues b) => EotSValues (Either a b) where
  eotSValues (Left  fields) = eotSValues fields
  eotSValues (Right _     ) = throw MultipleConstructorsError

-- | Turn the fields of an 'Eot' into 'SValue's.
instance (ToSValue f, EotSValues fs) => EotSValues (f, fs) where
  eotSValues (f, fs) = do
    let encodedFs = eotSValues fs
    toSValue f : encodedFs

instance EotSValues Eot.Void where
  eotSValues = Eot.absurd

instance EotSValues () where
  eotSValues () = []

-- | A list of fields names of a record.
fieldNames :: Eot.HasEot a => a -> [String]
fieldNames a = do
  case Eot.constructors $ Eot.datatype $ proxyByExample a of
    []  -> throw NoConstructorsError
    [c] -> case Eot.fields c of
      Eot.Selectors   fns -> fns
      Eot.NoSelectors _   -> throw NoSelectorsError
      Eot.NoFields        -> throw NoFieldsError
    _ -> throw MultipleConstructorsError

proxyByExample :: a -> Proxy a
proxyByExample _ = Proxy

-- * Convert from storable representation to bytestring.

-- | Convert an 'SDataType' to flattened 'Row's.
toRows :: SDataType -> Map.Map Table.TableKey Table.Table
toRows a = Map.fromList [(tableKey, table)]
  where table                  = Map.fromList [(rowKey, toRow fields)]
        (tableKey, rowKey)     = ref
        (SDataType ref fields) = a

-- | Convert 'SFields' to a table row.
toRow :: SFields -> Table.Row
toRow (SFields fieldsMap) = fmap encodeSValue fieldsMap

-- | Convert 'SValue' to a bytestring.
-- TODO: handle case of nested 'SDataType'.
encodeSValue :: SValue -> ByteString
encodeSValue (SValue prim) = Serialize.encode prim
