{-# LANGUAGE MonoLocalBinds        #-}

-- Conversion from table representation to data types.
module Telescope.Table.From where

import           Control.Exception         ( throw )
import           Telescope.Storable.From   ( FromSValues(..) )
import qualified Telescope.Exception      as E
import qualified Telescope.Table.Types    as Table
import           Telescope.Storable.Types  ( SValue(..) )
import           Text.Read                 ( readEither )

-- | A data type reconstructed from table representation.
aFromRow :: FromSValues a => Table.Row -> a
aFromRow = fromSValues . rowToSValues

-- | 'SValue's reconstructed from a row.
rowToSValues :: Table.Row -> [SValue]
rowToSValues row = [decodeSValue bs | (_, bs) <- row]

-- | An 'SValue' from a bytestring.
decodeSValue :: String -> SValue
decodeSValue prim =
  case readEither prim of
    Left  _ -> throw $ E.DeserializeException $
      "Could not deserialize the following into 'SValue':\n  " ++ show prim
    Right r -> SValuePrim r
