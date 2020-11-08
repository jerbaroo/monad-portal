{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}

module Telescope.Server where

import           Control.Monad                  ( forever, void )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.ByteString.Char8          ( ByteString, pack, unpack )
import           Data.Proxy                     ( Proxy(..) )
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.WebSockets            as WebSocket
import           Servant                        ( (:<|>)(..) )
import qualified Servant                       as Servant
import           Servant.Server.StaticFiles     ( serveDirectoryFileServer )
import qualified Telescope.Class               as Class
import qualified Telescope.Table               as Table
import           Telescope.DS.File              ( runT )
import qualified Telescope.Server.API          as API

viewTablesHandler :: [API.TableName] -> Servant.Handler API.Tables
viewTablesHandler tableNames = do
  tablesF <- runT $ Class.viewTables $ Class.toF $ map Table.TableKey tableNames
  liftIO $ putStrLn $ "Server: viewTables: " ++ show (Class.fromF tablesF)
  pure $ API.toAPITables $ Class.fromF tablesF

setTablesHandler :: API.Tables -> Servant.Handler Servant.NoContent
setTablesHandler apiTables = do
  runT $ Class.setTables $ Class.toF $ API.fromAPITables apiTables
  liftIO $ putStrLn $ "Server: setTables: " ++ show apiTables
  pure Servant.NoContent

rmTablesHandler :: [API.TableName] -> Servant.Handler Servant.NoContent
rmTablesHandler tableNames = do
  runT $ Class.rmTables $ Class.toF $ map Table.TableKey tableNames
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
      (Class.toF tableKey) (Class.toF rowKey) $ Class.toF $
        \maybeRow -> runT $ liftIO $ do
          putStrLn $ show $ maybeRow
          WebSocket.sendTextData conn $ pack $ show $ maybeRow
          putStrLn $ "WebSocket: update sent for " ++ show (tk, rk)

server :: Servant.Server API.API
server =
  (viewTablesHandler :<|> setTablesHandler :<|> rmTablesHandler)
  :<|> watchHandler
  :<|> serveDirectoryFileServer "build/demo-frontend/bin/demo-frontend.jsexe"

-- | Run a Telescope server.
run :: Int -> IO ()
run port = do
  putStrLn $ "Running server on port " ++ show port ++ " ..."
  Warp.run port $ Servant.serve (Proxy :: Proxy API.API) server
