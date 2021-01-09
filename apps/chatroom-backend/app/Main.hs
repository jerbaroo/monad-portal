{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           ChatRoom.Common          ( Message(Message) )
import           Data.Text                ( Text )
import           Data.Proxy               ( Proxy(Proxy) )
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant                  as Servant
import           Servant.API              ( (:<|>)((:<|>)), (:>), Get, JSON )
import           Telescope                ( primaryKey )
import qualified Telescope                as T
import           Telescope.DS.File        ( runT )
import qualified Telescope.Server         as Server
import qualified Telescope.Server.API     as Telescope

main :: IO ()
main = do
  let msg = Message 1 "main" "john" "Hello everyone"
  runT $ T.rmTable @Message >> T.set msg
  print =<< runT (T.viewK @Message $ primaryKey msg)
  -- If you only want to run Telescope's server then uncomment this line...
  -- Server.run 3002 Server.developmentCors
  -- ...otherwise the following code integrates with an additional server:
  Warp.run 3002 $ Server.developmentCors $
    Servant.serve (Proxy :: Proxy API) (additionalServer :<|> Server.server)

-- | Additional server with only one endpoint, for demonstrative purposes.
additionalServer :: Servant.Server AdditionalAPI
additionalServer = pure "This is data returned by the additional server"

type AdditionalAPI = "additional" :> Get '[JSON] Text
type API = AdditionalAPI :<|> Telescope.API
