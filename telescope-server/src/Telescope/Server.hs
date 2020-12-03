{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Telescope.Server where

import           Control.Comonad              ( extract )
import           Control.Monad                ( forever, void )
import           Control.Monad.IO.Class       ( liftIO )
import           Data.ByteString.Char8        ( ByteString, pack, unpack )
import           Data.Map                   as Map
import           Data.Set                   as Set
import           Data.Proxy                   ( Proxy(..) )
import           Network.Wai                  ( Middleware )
import qualified Network.Wai.Handler.Warp    as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Network.WebSockets          as WebSocket
import           Servant                      ( (:<|>)(..) )
import qualified Servant                     as Servant
import           Servant.Server.StaticFiles   ( serveDirectoryFileServer )
import qualified Telescope.Class             as Class
import qualified Telescope.Table.Types       as Table
import           Telescope.DS.File            ( runT )
import qualified Telescope.Server.API        as API
import qualified Telescope.Server.API.Types  as API

viewRowsHandler :: API.RowsIndex -> Servant.Handler API.Tables
viewRowsHandler apiRowsIndex = do
  rowsF <- runT $ Class.viewRows $ pure $ API.from apiRowsIndex
  pure $ API.to $ extract rowsF

viewTablesHandler :: [API.TableKey] -> Servant.Handler API.Tables
viewTablesHandler apiTableKeys = do
  tablesF <- runT $ Class.viewTables $ pure $ API.from apiTableKeys
  pure $ API.to $ extract tablesF

setRowsHandler :: API.Tables -> Servant.Handler Servant.NoContent
setRowsHandler apiRows = do
  runT $ Class.setRows $ pure $ API.from apiRows
  pure Servant.NoContent

setTablesHandler :: API.Tables -> Servant.Handler Servant.NoContent
setTablesHandler apiTables = do
  runT $ Class.setTables $ pure $ API.from apiTables
  pure Servant.NoContent

rmRowsHandler :: API.RowsIndex -> Servant.Handler Servant.NoContent
rmRowsHandler apiRowsIndex = do
  runT $ Class.rmRows $ pure $ API.from apiRowsIndex
  pure Servant.NoContent

rmTablesHandler :: [API.TableKey] -> Servant.Handler Servant.NoContent
rmTablesHandler apiTableKeys = do
  runT $ Class.rmTables $ pure $ API.from apiTableKeys
  pure Servant.NoContent

watchRowHandler :: WebSocket.Connection -> Servant.Handler ()
watchRowHandler conn = do
  liftIO $ void $ forever $ do
    message <- WebSocket.receiveData conn
    let (tableKey, rowKey) = read . unpack $ message :: Table.Ref
    runT $ Class.onChangeRow (pure (tableKey, rowKey)) $ pure $
      \maybeRow -> runT $ liftIO $ do
        putStrLn $ show maybeRow
        WebSocket.sendTextData conn $ pack $ show $ (rowKey, maybeRow)

watchTableHandler :: WebSocket.Connection -> Servant.Handler ()
watchTableHandler conn = do
  liftIO $ void $ forever $ do
    message <- WebSocket.receiveData conn
    let tableKey = read . unpack $ message :: Table.TableKey
    runT $ Class.onChangeTable (pure tableKey) $ pure $
      \table -> runT $ liftIO $ do
        WebSocket.sendTextData conn $ pack $ show $ (tableKey, table)

server :: Servant.Server API.API
server =
    (    viewRowsHandler
    :<|> viewTablesHandler
    :<|> setRowsHandler
    :<|> setTablesHandler
    :<|> rmRowsHandler
    :<|> rmTablesHandler
    )
  :<|>
    (    watchRowHandler
    :<|> watchTableHandler
    )
  :<|> serveDirectoryFileServer "build/todolist-frontend/bin/todolist-frontend.jsexe"

------------------
-- RUN A SERVER --
------------------

type Port = Int

-- | CORS policy useful when using separate backend and frontend servers.
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
