{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Telescope.Server.API.Types where

import           Data.Bifunctor         ( first, second )
import qualified Data.Map              as Map
import qualified Data.Set              as Set
import           Data.Set               ( Set )
import qualified Telescope.Table.Types as Table'

type RowIndices = [(TableKey, [Table'.RowKey])]
type TableKey   = String
type Table      = (TableKey, [(Table'.RowKey, Table'.Row)])
type Tables     = [Table]

-- | A datatype that can be converted to/from API format.
class APIFormat a b | a -> b, b -> a where
  to   :: a -> b
  from :: b -> a

instance APIFormat Table'.RowIndices RowIndices where
  to   = map (first Table'.unTableKey . second Set.toList) . Map.toList
  from = Map.fromList . map (first Table'.TableKey . second Set.fromList)

instance APIFormat Table'.Tables Tables where
  to   = map (first Table'.unTableKey) . Map.toList . fmap Map.toList
  from = fmap Map.fromList . Map.fromList . map (first Table'.TableKey)

instance APIFormat (Set Table'.TableKey) [TableKey] where
  to   = map Table'.unTableKey . Set.toList
  from = Set.fromList . map Table'.TableKey
