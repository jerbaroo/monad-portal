{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Telescope.DS.Reflex.Dom where

import           Control.Bool                ( guard' )
import           Control.Monad               ( void )
import           Control.Monad.IO.Class      ( liftIO )
import           Data.Text                   ( Text, pack, unpack )
import qualified Data.Text.Encoding         as Text
import           Reflex.Dom
import qualified Telescope.Server.API.Types as API
import           Telescope.Class             ( Telescope(..) )

import qualified Telescope.Table.Types      as Table

rootURL   :: Text
wsRootURL :: Text

rootURL     = "http://localhost:3002"
wsRootURL   = "ws://localhost:3002"
watchRowURL = wsRootURL <> "/watchRow"
watchTableURL = wsRootURL <> "/watchTable"

instance ( Functor m , MonadWidget t m , Reflex t )
  => Telescope m (Event t) where

  viewRows rowsIndexEvn = do
    -- Perform XHR requests..
    let toRequest = postJson (rootURL <> "/viewRows") . API.to
    responseEvent <- performRequestAsync $ toRequest <$> rowsIndexEvn
    -- ..and decode responses.
    let apiTables :: Event t (API.Tables)
        apiTables = mapMaybe id $ decodeXhrResponse <$> responseEvent
    pure $ API.from <$> apiTables

  viewTables tableKeyEvn = do
    -- Perform XHR requests..
    let toRequest = postJson (rootURL <> "/viewTables") . API.to
    responseEvent <- performRequestAsync $ toRequest <$> tableKeyEvn
    -- ..and decode responses.
    let apiTables :: Event t (API.Tables)
        apiTables = mapMaybe id $ decodeXhrResponse <$> responseEvent
    pure $ API.from <$> apiTables

  setRows rowsEvn = do
    let toRequest = postJson (rootURL <> "/setRows") . API.to
    void $ performRequestAsync $ toRequest <$> rowsEvn

  setTables tablesEvn = do
    let toRequest = postJson (rootURL <> "/setTables") . API.to
    void $ performRequestAsync $ toRequest <$> tablesEvn

  rmRows rowsIndexEvn = do
    let toRequest = postJson (rootURL <> "/rmRows") . API.to
    void $ performRequestAsync $ toRequest <$> rowsIndexEvn

  rmTables tableKeyEvn = do
    let toRequest = postJson (rootURL <> "/rmTables") . API.to
    void $ performRequestAsync $ toRequest <$> tableKeyEvn

  updateable = pure True

  update original changes = pure $ leftmost [original, changes]

  watchRow refEvn = do
    -- Send subscription request over WebSocket..
    ws <- webSocket watchRowURL $ def & webSocketConfig_send .~
      ((\(tk, rk) -> [pack $ show (tk, rk)]) <$> refEvn)
    let responses = read . unpack . Text.decodeUtf8 <$> _webSocket_recv ws
    -- And filter out any out-of-date responses..
    refMayDyn <- current <$> holdDyn Nothing (Just <$> refEvn)
    pure $ snd <$> attachWithMaybe
      (\refMay response -> maybe (Just response)
        (\ref -> guard' (snd ref == fst response) response) refMay)
      refMayDyn responses

  watchTable tableKeyEvn = do
    -- Send subscription request over WebSocket..
    ws <- webSocket watchTableURL $ def & webSocketConfig_send .~
      ((\tk -> [pack $ show tk]) <$> tableKeyEvn)
    let responses = read . unpack . Text.decodeUtf8 <$> _webSocket_recv ws
    -- And filter out any out-of-date responses..
    logEvent responses $ \t -> "Received watchTables update: " ++ show t
    tableKeyMayDyn <- current <$> holdDyn Nothing (Just <$> tableKeyEvn)
    pure $ snd <$> attachWithMaybe
      (\tableKeyMay response -> maybe (Just response)
        (\tableKey -> guard' (tableKey == fst response) response) tableKeyMay)
      tableKeyMayDyn responses

-- | Logs a string to the console when an event fires.
--
-- Differs slightly from 'Reflex.traceEvent' because it will print even if the
-- event is otherwise unused. Copied from the reflex-dom-contrib package.
logEvent :: MonadWidget t m => Event t a -> (a -> String) -> m ()
logEvent e mkStr = performEvent_ (liftIO . putStrLn . mkStr <$> e)
