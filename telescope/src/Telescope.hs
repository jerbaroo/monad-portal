{-|
Module      : Telescope
Description : User-facing module that re-exports useful functionality.

Suggested imports:
>  import           Telescope  ( PrimaryKey(..) )
>  import qualified Telescope as T
-}
module Telescope
  ( module Telescope.Operations
  , PrimaryKey
  , primaryKey
  ) where

import Telescope.Operations
import Telescope.Class      ( PrimaryKey, primaryKey )
