{-| Library users should import this module.

Suggested usage:
>  import           Telescope  ( PrimaryKey(..) )
>  import qualified Telescope as T
-}
module Telescope
  ( module Telescope.Operations
  , PrimaryKey
  , primaryKey
  ) where

import Telescope.Operations
import Telescope.Table.To   ( PrimaryKey, primaryKey )
