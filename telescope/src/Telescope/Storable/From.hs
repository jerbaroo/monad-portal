{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

-- Reconstruction of data types from storable representation via 'Generics.Eot'.
module Telescope.Storable.From where

import           Control.Exception         ( throw )
import           Data.Text                 ( Text, unpack )
import qualified Generics.Eot             as Eot
import qualified Telescope.Exception      as E
import           Telescope.Storable.Types  ( SValue(..) )
import qualified Telescope.Table.Types    as Table

-- This module provides functions and type classes for the reconstruction of
-- data types from storable representation. This conversion is accomplished via
-- the 'Generics.Eot' library. See 'Telescope.Storable' for a description of the
-- storable representation used by the Telescope framework.

--------------------------------------------------------------------------------
-- FromSValue ------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Reconstruction of a fields's value from storable representation. ------------
--------------------------------------------------------------------------------

-- | A field's value that can be reconstructed from storable representation.
class FromSValue a where
  fromSValue :: SValue -> a

class FromPrim a where
  fromPrim :: Table.Prim -> a

instance FromPrim Text where
  fromPrim (Table.PText a) = a

-- | A field's value that is an 'Int' primitive.
instance FromSValue Int where
  fromSValue (SValuePrim (Table.PInt a)) = a
  fromSValue s = throw $ E.DeserializeException $
    "Can't deserialize the following into 'Int':\n  " ++ show s

-- | A field's value that is a 'Text' primitive.
instance FromSValue Text where
  fromSValue (SValuePrim (Table.PText a)) = a
  fromSValue s = throw $ E.DeserializeException $
    "Can't deserialize the following into 'Text':\n  " ++ show s

-- | A field's value that is a list of primitives.
instance FromPrim a => FromSValue [a] where
  fromSValue (SValuePrim (Table.PText a)) =
    let primList :: [Table.Prim]
        primList = read $ unpack a
    in (fmap (fromPrim @a) primList)
  fromSValue s = throw $ E.DeserializeException $
    "Can't deserialize the following into '[a]':\n  " ++ show s

instance FromSValue () where
  fromSValue _ = ()

--------------------------------------------------------------------------------
-- FromSValues -----------------------------------------------------------------
--------------------------------------------------------------------------------
-- Reconstruction of a data type from storable representation. -----------------
--------------------------------------------------------------------------------

-- | A data type that can be reconstructed from storable representation.
class FromSValues a where
  fromSValues :: [SValue] -> a

-- | Derive an instance of 'FromSValues' via Generics.
instance (Eot.HasEot a, EotFromSValues (Eot.Eot a)) => FromSValues a where
  fromSValues = Eot.fromEot . eotFromSValues

-- | 'Generics.Eot' implementation of 'FromSValues'.
class EotFromSValues eot where
  eotFromSValues :: [SValue] -> eot

-- | Reconstruct 'Either's from 'SValue's.
--
-- All the 'SValue's represent the values of the different fields of a a single
-- data constructor. This is because Telescope does not currently support data
-- types with multiple constructors. So we will never return a 'Right' which
-- represents additional constructors. Only a single 'Left' will be returned.
instance (EotFromSValues l, EotFromSValues r)
    => EotFromSValues (Either l r) where
  eotFromSValues [] = throw E.NoConstructorsException
  eotFromSValues as = Left $ eotFromSValues as

-- | Reconstruct right-nested tuples from ordered 'SValue's.
--
-- Each left value of the 2-tuple represents one field of a data type. The
-- 2-tuples are right-nested, so the remaining fields are nested in 'fs'.
instance (FromSValue f, EotFromSValues fs) => EotFromSValues (f, fs) where
  eotFromSValues (f:fs) = (fromSValue f, eotFromSValues fs)
  eotFromSValues []     = throw E.NoFieldsException

-- | No more 'SValue's to convert, this is the end of the right-nested tuples.
instance EotFromSValues () where
  eotFromSValues []    = ()
  eotFromSValues (_:_) =
    throw $ E.DeserializeException "Eot error constructing ()"

-- | Necessary boilerplate.
instance EotFromSValues Eot.Void where
  eotFromSValues _ =
    throw $ E.DeserializeException "Eot error constructing Void"
