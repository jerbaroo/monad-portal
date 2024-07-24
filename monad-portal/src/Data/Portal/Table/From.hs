-- | Conversion from table representation.
module Data.Portal.Table.From where

import Control.Exception (throw)
import Data.Int (Int64)
import Data.Text as Text
import Data.Portal.Exception qualified as E
import Data.Portal.Storable.Types qualified as Storable
import Data.Portal.Table.Types qualified as Table

-- | 'SValue's reconstructed from a row.
--
-- TODO: https://github.com/jerbaroo/new-telescope/issues/17
-- This conversion only handles 'SValue's that are primitives.
-- Converstion of nested data types does not yet work.
rowToSValues :: Table.Row -> [Storable.SValue]
rowToSValues row = [Storable.SValuePrim prim | (_, prim) <- row]

class FromRows a b where
  fromRows :: Table.Rows -> a -> b

instance FromRows Table.PrimNotNull Bool where
  fromRows _ (Table.PrimBool b) = b
  fromRows _ x = fromRowsErr "Bool" x

instance FromRows Table.PrimNotNull Int64 where
  fromRows _ (Table.PrimInt i) = i
  fromRows _ x = fromRowsErr "Int" x

instance FromRows Table.PrimNotNull Text where
  fromRows _ (Table.PrimText t) = t
  fromRows _ x = fromRowsErr "Text" x

-- instance FromRows Table.PrimNotNull a => FromRows Table.PrimNotNull [a] where
--   fromRows _ (Table.PrimText t) = Table.primRead $ Text.unpack t
--   fromRows _ x = fromRowsErr "[a]" x

instance FromRows Table.PrimNotNull a => FromRows Table.Prim (Maybe a) where
  fromRows _ Table.PrimNull = Nothing
  fromRows r (Table.PrimNotNull p) = Just $ fromRows r p

class FromPrim a where
  fromPrim :: Table.Prim -> a

instance FromPrim Bool where
  fromPrim (Table.PrimNotNull (Table.PrimBool a)) = a
  fromPrim s                                      = fromRowsErr "Bool" s

instance FromPrim Int64  where
  fromPrim (Table.PrimNotNull (Table.PrimInt a))  = a
  fromPrim s                                      = fromRowsErr "Int" s

instance FromPrim Text where
  fromPrim (Table.PrimNotNull (Table.PrimText a)) = a
  fromPrim s                                      = fromRowsErr "Text" s

instance FromPrim a => FromPrim (Maybe a) where
  fromPrim Table.PrimNull                         = Nothing
  fromPrim a                                      = Just $ fromPrim a

fromRowsErr :: Show a => String -> a -> e
fromRowsErr a s = throw $ E.DeserializeException $
  -- TODO: improve message with TypeApplications.
  "Can't deserialize " ++ a ++ ":\n  " ++ show s
