{-# LANGUAGE MonoLocalBinds        #-}

-- Conversion from table to storable representation.
module Telescope.Table.From where

import           Control.Exception         ( throw )
import           Data.Text                as Text
import qualified Telescope.Exception      as E
import qualified Telescope.Storable.Types as Storable
import qualified Telescope.Table.Types    as Table

-- | 'SValue's reconstructed from a row.
--
-- TODO: https://github.com/jerbaroo/new-telescope/issues/17
-- This conversion only handles 'SValue's that are primitives.
-- Converstion of nested data types does not yet work.
rowToSValues :: Table.Row -> [Storable.SValue]
rowToSValues row = [Storable.SValuePrim prim | (_, prim) <- row]

--------------------------------------------------------------------------------
-- Conversion of primitives ----------------------------------------------------
--------------------------------------------------------------------------------

class FromPrim a where
  fromPrim :: Table.Prim -> a

instance FromPrim Bool where
  fromPrim (Table.PrimNotNull (Table.PrimBool a)) = a
  fromPrim s                                      = fromPrimErr "Bool" s
instance FromPrim Int  where
  fromPrim (Table.PrimNotNull (Table.PrimInt a))  = a
  fromPrim s                                      = fromPrimErr "Int" s
instance FromPrim Text where
  fromPrim (Table.PrimNotNull (Table.PrimText a)) = a
  fromPrim s                                      = fromPrimErr "Text" s
instance FromPrim a => FromPrim (Maybe a) where
  fromPrim Table.PrimNull                         = Nothing
  fromPrim a                                      = Just $ fromPrim a

fromPrimErr :: Show a => String -> a -> e
fromPrimErr a s = throw $ E.DeserializeException $
  -- TODO: improve message with TypeApplications.
  "Can't deserialize FromPrim to " ++ a ++ ":\n  " ++ show s
