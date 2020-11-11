{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Telescope.Server where

import           Control.Comonad              ( extract )
import           Control.Monad                ( forever, void )
import           Control.Monad.IO.Class       ( liftIO )
import           Data.ByteString.Char8        ( ByteString, pack, unpack )
import           Data.Map                   as Map
import           Data.Proxy                   ( Proxy(..) )
import           Network.Wai                  ( Middleware )
import qualified Network.Wai.Handler.Warp    as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Network.WebSockets          as WebSocket
import           Servant                      ( (:<|>)(..) )
import qualified Servant                     as Servant
import           Servant.Server.StaticFiles   ( serveDirectoryFileServer )
import qualified Telescope.Class             as Class
import qualified Telescope.Table             as Table
import           Telescope.DS.File            ( runT )
import qualified Telescope.Server.API        as API

viewTablesHandler :: [API.TableKey] -> Servant.Handler API.Tables
viewTablesHandler apiTableKeys = do
  let tableKeys = API.fromAPITableKeys apiTableKeys
  tablesF <- runT $ Class.viewTables $ pure $ tableKeys
  liftIO $ putStrLn $ "\nServer: viewTables: " ++ show (extract tablesF)
  pure $ API.toAPITables $ extract tablesF

setRowsHandler :: API.Tables -> Servant.Handler Servant.NoContent
setRowsHandler apiRows = do

  -- BEGIN FOR DEBUGGGING
  let tables :: Table.Tables
      tables = API.fromAPITables apiRows
      tableKeys :: [Table.TableKey]
      tableKeys = Map.keys tables
  tablesF <- runT $ Class.viewTables $ pure tableKeys
  liftIO $ putStrLn $ "\nServer: setTables: (viewed) " ++ show (extract tablesF)
  -- END FOR DEBUGGGING

  runT $ Class.setRows $ pure $ API.fromAPITables apiRows
  liftIO $ putStrLn $ "\nServer: setRows: " ++ show apiRows
  pure Servant.NoContent

setTablesHandler :: API.Tables -> Servant.Handler Servant.NoContent
setTablesHandler apiTables = do
  runT $ Class.setTables $ pure $ API.fromAPITables apiTables
  liftIO $ putStrLn $ "\nServer: setTables: (toSet)" ++ show apiTables
  pure Servant.NoContent

rmTablesHandler :: [API.TableKey] -> Servant.Handler Servant.NoContent
rmTablesHandler apiTableKeys = do
  runT $ Class.rmTables $ pure $ API.fromAPITableKeys apiTableKeys
  pure Servant.NoContent

type Sub = (Table.TableKey, Table.RowKey)

watchHandler :: WebSocket.Connection -> Servant.Handler ()
watchHandler conn = do
  liftIO $ putStrLn "Websocket: connection opened"
  liftIO $ void $ forever $ do
    message <- WebSocket.receiveData conn :: IO ByteString
    let (tableKey, rowKey) = read . unpack $ message :: Sub
        (Table.TableKey tk, Table.RowKey rk) = (tableKey, rowKey)
        confirmation = "WebSocket: new subscription for " ++ show (tk, rk)
    WebSocket.sendTextData conn $ pack confirmation
    putStrLn $ confirmation
    runT $ Class.onChangeRow
      (pure tableKey) (pure rowKey) $ pure $
        \maybeRow -> runT $ liftIO $ do
          putStrLn $ show $ maybeRow
          WebSocket.sendTextData conn $ pack $ show $ maybeRow
          putStrLn $ "WebSocket: update sent for " ++ show (tk, rk)

server :: Servant.Server API.API
server =
  (viewTablesHandler :<|> setRowsHandler :<|> setTablesHandler :<|> rmTablesHandler)
  :<|> watchHandler
  :<|> serveDirectoryFileServer "build/demo-frontend/bin/demo-frontend.jsexe"

------------------
-- RUN A SERVER --
------------------

type Port = Int

-- | CORS policy useful when using a separate frontend server.
developmentCors :: Middleware
developmentCors = Cors.cors $ const $ Just Cors.simpleCorsResourcePolicy
  { Cors.corsMethods        = ["OPTIONS", "POST"]
  , Cors.corsRequestHeaders = ["Content-Type"]
  }

-- | Run a Telescope server.
run :: Port -> Middleware -> IO ()
run port middleware = do
  putStrLn $ "Running Telescope server on port " ++ show port
  Warp.run port
    $ middleware
    $ Servant.serve (Proxy :: Proxy API.API) server
