{-| Exports for library users.

Suggested usage:
>  import Data.Portal (PrimaryKey(..))
>  import Data.Portal qualified as P
-}
module Data.Portal (module X) where

import Data.Portal.Operations as X
import Data.Portal.Table.To as X (PrimaryKey(..))
