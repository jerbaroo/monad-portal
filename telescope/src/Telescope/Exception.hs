{-| Errors in Telescope execution. -}
module Telescope.Exception where

import Control.Exception ( Exception )

-- | An exception that occurs within Telescope.
data TelescopeException =
    -- A data type must have exactly one constructor.
    MultipleConstructorsException
    -- A data type must have exactly one constructor.
  | NoConstructorsException
    -- A data type must have fields to serialize.
  | NoFieldsException
    -- A data type must have fields with selectors (names).
  | NoSelectorsException
    -- Could not deserialize a primitive.
  | DeserializeException String
    -- Could not serialize a primitive.
  | SerializeException String
    -- Telescope type class instance badly implemented.
  | InstanceException String
  deriving Show

instance Exception TelescopeException
