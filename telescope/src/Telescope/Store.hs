{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Telescope.Store where

import           Control.Exception              ( throw )
import qualified Data.Foldable                 as Foldable
import qualified Data.Map                      as Map
import           Data.Proxy                     ( Proxy(Proxy) )
import qualified Generics.Eot                  as Eot
import           GHC.Generics                   ( Generic )
import qualified Telescope.Exception           as T
import qualified Telescope.Table               as Table
import           Text.Read                      ( readEither )

-- A storable representation of a data type and fields.
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
  -- A primitive storable value.
  SValue Table.Prim
  -- One nested storable data type.
  | SDT SDataType
  -- A nested list (from any 'Foldable') of storable data types.
  | SDTs [SDataType]
  deriving (Generic, Show)

-- | A storable representation of ALL fields of a data type.
newtype SFields = SFields [(Table.ColumnKey, SValue)]
  deriving (Generic, Show)

-- | A storable representation of a data type (type and fields).
data SDataType = SDataType Table.Ref SFields
  deriving (Generic, Show)

-- Typeclasses and instances for converting to storable representation.

-- | Convert a field's value to a storable representation.
class ToSValue a where
  toSValue :: a -> SValue

instance ToSValue Int where    toSValue a = SValue $ Table.PInt a
instance ToSValue String where toSValue a = SValue $ Table.PString a

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
    in  SFields $ zipWith (\n v -> (Table.ColumnKey n, v)) names values

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
instance {-# OVERLAPPABLE #-}
  (Table.HasTableKey a, Table.HasRowKey a, ToSFields a)
  => ToSDataType a where

-- Generics-Eot implementations of 'EotSValues' and 'fieldNames'.

class EotSValues eot where
  eotSValues :: eot -> [SValue]

-- | Error in case of a 'Right', only operate on the first constructor.
instance (EotSValues a, EotSValues b) => EotSValues (Either a b) where
  eotSValues (Left  fields) = eotSValues fields
  eotSValues (Right _     ) = throw T.MultipleConstructorsException

-- | Turn the fields of an 'Eot' into 'SValue's.
instance (ToSValue f, EotSValues fs) => EotSValues (f, fs) where
  eotSValues (f, fs) =
    let encodedFs = eotSValues fs
    in toSValue f : encodedFs

instance EotSValues Eot.Void where
  eotSValues = Eot.absurd

instance EotSValues () where
  eotSValues () = []

-- | A list of fields names of a record.
fieldNames :: Eot.HasEot a => a -> [String]
fieldNames a = do
  case Eot.constructors $ Eot.datatype $ proxyByExample a of
    []  -> throw T.NoConstructorsException
    [c] -> case Eot.fields c of
      Eot.Selectors   fns -> fns
      Eot.NoSelectors _   -> throw T.NoSelectorsException
      Eot.NoFields        -> throw T.NoFieldsException
    _ -> throw T.MultipleConstructorsException

proxyByExample :: a -> Proxy a
proxyByExample _ = Proxy

-- Reconstruct a data type from 'SValue's.

class EotFromSValues eot where
  eotFromSValues :: [SValue] -> eot

-- | Construct 'Either's from 'SValue's.
--
-- Only data types with a single constructor are supported. So we will never
-- construct a 'Right' which represents additional constructors. Only a single
-- 'Left' will be returned.
instance (EotFromSValues l, EotFromSValues r)
  => EotFromSValues (Either l r) where
  eotFromSValues [] = throw T.NoConstructorsException
  eotFromSValues as = Left $ eotFromSValues as

-- | Construction of right-nested tuples from each ordered 'SValue'.
instance (FromSValue a, EotFromSValues as) => EotFromSValues (a, as) where
  eotFromSValues (a:as) = (fromSValue a, eotFromSValues as)
  eotFromSValues [] = throw T.NoFieldsException

instance EotFromSValues Eot.Void where
  eotFromSValues _ = throw $ T.DeserializeException "Eot error constructing Void"

instance EotFromSValues () where
  eotFromSValues [] = ()
  eotFromSValues (_:_) = throw $ T.DeserializeException "Eot error constructing ()"

class FromSValue a where
  fromSValue :: SValue -> a

instance FromSValue Int where
  fromSValue (SValue (Table.PInt a)) = a
  fromSValue s = throw $ T.DeserializeException $
    "Can't deserialize the following into 'Int':\n  " ++ show s

instance FromSValue String where
  fromSValue (SValue (Table.PString a)) = a
  fromSValue s = throw $ T.DeserializeException $
    "Can't deserialize the following into 'String':\n  " ++ show s

instance FromSValue () where
  fromSValue _ = ()

-- | A data type that can be reconstructed from 'SValue's.
class FromSValues a where
  fromSValues :: [SValue] -> a

-- | Derive an instance of 'FromSValues' via Generics.
instance (Eot.HasEot a, EotFromSValues (Eot.Eot a)) => FromSValues a where
  fromSValues = Eot.fromEot . eotFromSValues

-- Encode/decode to/from bytestring.

-- Encode (to bytestring).

-- | Convert an 'SDataType' to flattened 'Row's.
-- TODO: make this take an 'a' not 'SDataType'.
toRows :: SDataType -> Map.Map Table.TableKey Table.Table
toRows a = Map.fromList [(tableKey, table)]
  where table                  = Map.fromList [(rowKey, sFieldsToRow fields)]
        (tableKey, rowKey)     = ref
        (SDataType ref fields) = a

-- | Convert 'SFields' to a table row.
sFieldsToRow :: SFields -> Table.Row
sFieldsToRow (SFields fieldsMap) =
  [ (columnKey, encodeSValue sValue)
  | (columnKey, sValue) <- fieldsMap
  ]

-- | Convert 'SValue' to a bytestring.
-- TODO: handle case of nested 'SDataType'.
encodeSValue :: SValue -> String
encodeSValue (SValue prim) = show prim

-- Decode (from bytestring).

-- | 'SValue's reconstructed from a row.
rowToSValues :: Table.Row -> [SValue]
rowToSValues row = [decodeSValue bs | (_, bs) <- row]

-- | An 'SValue' from a bytestring.
decodeSValue :: String -> SValue
decodeSValue prim =
  case readEither prim of
    Left  _ -> throw $ T.DeserializeException $
      "Could not deserialize the following into 'SValue':\n  " ++ show prim
    Right r -> SValue r

-- | A data type reconstructed from a row.
fromRow :: FromSValues a => Table.Row -> a
fromRow = fromSValues . rowToSValues
