{-| Re-exports for library users.

Suggested usage:
>  import           Data.Portal  ( PrimaryKey(..) )
>  import qualified Data.Portal as T
-}
module Data.Portal (module X) where

import Data.Portal.Operations as X
import Data.Portal.Table.To   as X ( PrimaryKey, primaryKey )