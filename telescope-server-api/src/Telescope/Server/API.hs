{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Telescope.Server.API where

import           Data.Map    ( Map )
import           Servant.API ( (:<|>), (:>), Capture, DeleteNoContent, Get,
                              JSON, NoContent, PostNoContent, ReqBody )

-- | A list of tuples of: RowKey and Row.
type TableAsList = [(String, String)]

type API =
  "viewTable"
    :> Capture "tableKey" String
    :> Get '[JSON] TableAsList
  :<|>
  "setTable"
    :> Capture "tableKey" String
    :> ReqBody '[JSON] TableAsList
    :> PostNoContent
  :<|>
  "rmTable"
    :> Capture "tableKey" String
    :> DeleteNoContent
