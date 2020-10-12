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
import qualified Telescope.Class               as Class
import qualified Telescope.Table               as Table
import           Telescope.DS.File              ( runTFile )
import qualified Telescope.Server.API          as API

server :: Servant.Server API.API
server =
  (viewTableHandler :<|> setTableHandler :<|> rmTableHandler) :<|> watchHandler

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
    runTFile $ Class.onChangeRow tableKey rowKey $
      \row -> runTFile $ liftIO $ do
        putStrLn $ show $ row
        WebSocket.sendTextData conn $ pack $ show $ row
        putStrLn $ "WebSocket: update sent for " ++ show (tk, rk)

-- | Run a Telescope server.
run :: Int -> IO ()
run port = do
  putStrLn $ "Running server on port " ++ show port ++ " ..."
  Warp.run port $ Servant.serve (Proxy :: Proxy API.API) server
