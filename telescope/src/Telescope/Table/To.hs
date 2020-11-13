{-# LANGUAGE MonoLocalBinds        #-}

-- Conversion data types to table representation.
module Telescope.Table.To where

import qualified Data.Map                 as Map
import           Telescope.Storable.To     ( ToSDataType )
import qualified Telescope.Storable.To    as Storable
import           Telescope.Storable.Types  ( SDataType(..), SFields(..),
                                             SValue(..) )
import qualified Telescope.Table.Types    as Table

-- | From data type to table representation.
aToRows :: ToSDataType a k => a -> Map.Map Table.TableKey Table.Table
aToRows = sToRows . Storable.toSDataType

-- | From storable representation to table representation.
sToRows :: SDataType -> Map.Map Table.TableKey Table.Table
sToRows a = Map.fromList [(tableKey, table)]
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
encodeSValue :: SValue -> String
encodeSValue (SValuePrim prim) = show prim
