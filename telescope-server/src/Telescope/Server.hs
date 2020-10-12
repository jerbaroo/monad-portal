{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Telescope.Server where

import qualified Data.Map                 as Map
import           Data.Proxy                ( Proxy(..) )
import qualified Network.Wai.Handler.Warp as Warp
import           Servant                   ( (:<|>)(..) )
import qualified Servant                  as Servant
import qualified Telescope.Class          as Class
import qualified Telescope.Table          as Table
import           Telescope.DS.File         ( runTFile )
import           Telescope.Server.API     as API

server :: Servant.Server API
server = viewTableHandler :<|> setTableHandler :<|> rmTableHandler

viewTableHandler :: String -> Servant.Handler API.TableAsList
viewTableHandler tableKey = do
  rows <- runTFile $ Class.viewTableRows $ Table.TableKey tableKey
  pure $ map (\(a, b) -> (show a, show b)) $ Map.toList rows

setTableHandler :: String -> API.TableAsList -> Servant.Handler Servant.NoContent
setTableHandler tableKey rowStrings = do
  let rows :: [(Table.RowKey, Table.Row)]
      rows = map (\(a, b) -> (read a, read b)) rowStrings
  runTFile $ Class.setTableRows (Table.TableKey tableKey) $ Map.fromList rows
  pure Servant.NoContent

rmTableHandler :: String -> Servant.Handler Servant.NoContent
rmTableHandler tableKey = do
  runTFile $ Class.rmTableRows $ Table.TableKey tableKey
  pure Servant.NoContent

-- | Run a Telescope server.
run :: Int -> IO ()
run port = do
  putStrLn $ "Running server on port " ++ show port ++ " ..."
  Warp.run port $ Servant.serve (Proxy :: Proxy API) server
