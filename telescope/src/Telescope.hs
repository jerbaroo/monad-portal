{-|
Module      : Telescope
Description : User-facing module that re-exports from other modules.
-}
module Telescope
  ( module Telescope.Operations
  , Telescope
  , PrimaryKey
  , primaryKey
  ) where

import Telescope.Operations
import Telescope.Class      ( Telescope, PrimaryKey, primaryKey )
