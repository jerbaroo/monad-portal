-- | Conversion to table representation.
module Data.Portal.Table.To where

import qualified Data.Map as Map
import Data.Proxy ( Proxy(..) )
import Data.Foldable ( foldl' )
import Data.Text ( Text )
import Data.Typeable ( Typeable, typeRep )
import Data.Portal.Storable.Types ( SDataType(..), SField, SFields(..), SValue(..) )
import Data.Portal.Table.Types qualified as Table

-- | Types that can be converted to a 'PrimNotNull'.
class ToPrimNotNull a where
  toPrimNotNull :: a -> Table.PrimNotNull

instance ToPrimNotNull Bool where toPrimNotNull = Table.PrimBool
instance ToPrimNotNull Int  where toPrimNotNull = Table.PrimInt
instance ToPrimNotNull Text where toPrimNotNull = Table.PrimText

-- | Types that can be converted to a 'Prim'.
class ToPrim a where
  toPrim :: a -> Table.Prim

instance {-# OVERLAPPABLE #-} ToPrimNotNull a => ToPrim a where
  toPrim = Table.PrimNotNull . toPrimNotNull

instance ToPrim a => ToPrim (Maybe a) where
  toPrim Nothing  = Table.PrimNull
  toPrim (Just a) = toPrim a

-- | Types that have a 'TableKey'.
class HasTableKey a where
  tableKey :: Table.TableKey

instance Typeable a => HasTableKey a where
  tableKey = Table.TableKey $ show $ typeRep $ Proxy @a

-- | Types that have a 'TableKey'.
class HasRowKey a where
  rowKey :: a -> Table.RowKey

-- | Convenient way for library users to implement 'RowKey', with automatic
-- conversion from 'k' to 'Table.RowKey'.
class ToRowKey k => PrimaryKey a k | a -> k where
  primaryKey :: a -> k

instance PrimaryKey a k => HasRowKey a where
  rowKey = toRowKey . primaryKey

-- | Types that can be converted to a 'RowKey'.
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

-- | Convert from storable to table representation.
sToRows :: SDataType -> Table.Rows
sToRows (SDataType ((tk, rk), fields)) = do
  let (row, nestedRows) = sFieldsToRow fields
  unionRows [Map.singleton tk $ Map.singleton rk row, nestedRows]

-- | Convert storable fields into a row, and any nested rows.
sFieldsToRow :: SFields -> (Table.Row, Table.Rows)
sFieldsToRow (SFields fields) = do
  -- For each field determine the storable column, and any nested rows.
  let colsAndNestedRows = sFieldToCol <$> fields
  -- Transpose so first item is column values, and second is nested rows.
  (fst <$> colsAndNestedRows, unionRows $ snd <$> colsAndNestedRows)

-- | Convert a storable field into a row, and any nested rows.
sFieldToCol :: SField -> ((Table.ColumnKey, Table.Prim), Table.Rows)
sFieldToCol (colKey, SValuePrim prim) = ((colKey, prim), Map.empty)
sFieldToCol (colKey, SValueDataType (SDataType (ref@(tk, rk), fields))) =
  let (row, nestedRows) = sFieldsToRow fields
  in  ( (colKey, Table.PrimNotNull $ Table.PrimRef ref)
      , unionRows [Map.singleton tk $ Map.singleton rk row, nestedRows]
      )

unionRows :: [Table.Rows] -> Table.Rows
unionRows = foldl' (Map.unionWith Map.union) Map.empty
