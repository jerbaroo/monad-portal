{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}

module Telescope.Server where

import           Control.Monad                  ( forever, void )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.ByteString.Char8          ( ByteString, pack, unpack )
import qualified Data.Map                      as Map
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

viewTableHandler :: String -> Servant.Handler API.TableAsList
viewTableHandler tableKey = do
  fRows <- runT $ Class.viewTableRows $ Class.toF $ Table.TableKey tableKey
  pure $ map (\(a, b) -> (show a, show b)) $ Map.toList $ Class.fromF fRows

setTableHandler :: String -> API.TableAsList -> Servant.Handler Servant.NoContent
setTableHandler tableKey rowStrings = do
  let rows :: [(Table.RowKey, Table.Row)]
      rows = map (\(a, b) -> (read a, read b)) rowStrings
  runT $ Class.setTableRows
    (Class.toF $ Table.TableKey tableKey)
    (Class.toF $ Map.fromList rows)
  pure Servant.NoContent

rmTableHandler :: String -> Servant.Handler Servant.NoContent
rmTableHandler tableKey = do
  runT $ Class.rmTableRows $ Class.toF $ Table.TableKey tableKey
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
  (viewTableHandler :<|> setTableHandler :<|> rmTableHandler)
  :<|> watchHandler
  :<|> serveDirectoryFileServer "build/demo-frontend/bin/demo-frontend.jsexe"

-- | Run a Telescope server.
run :: Int -> IO ()
run port = do
  putStrLn $ "Running server on port " ++ show port ++ " ..."
  Warp.run port $ Servant.serve (Proxy :: Proxy API.API) server
