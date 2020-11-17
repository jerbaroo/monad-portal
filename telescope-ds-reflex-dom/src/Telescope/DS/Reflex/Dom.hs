{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE UndecidableInstances  #-}

module Telescope.DS.Reflex.Dom where

import           Control.Monad               ( void )
import           Control.Monad.IO.Class      ( liftIO )
import qualified Data.Map                   as Map
import           Data.Text                   ( Text, pack )
import           Reflex.Dom
import           Telescope.Server.API.Types as API
import           Telescope.Class             ( Telescope(..) )

rootURL :: Text
rootURL = "http://localhost:3002"

instance
  ( Functor m
  , MonadWidget t m
  , Applicative (Dynamic t)
  , Reflex t
  ) => Telescope m (Dynamic t) where

  viewRows rowsIndexDyn = do
    -- Perform XHR requests..
    let toRequest = postJson (rootURL <> "/viewRows") . API.to
    responseEvent <- performRequestAsync $ toRequest <$> updated rowsIndexDyn
    -- ..and decode responses.
    let apiTables :: Event t (API.Tables)
        apiTables = mapMaybe id $ decodeXhrResponse <$> responseEvent
    logEvent apiTables $ \t -> "DOM-viewRows: " ++ show t
    holdDyn Map.empty $ API.from <$> apiTables

  viewTables tableKeysDyn = do
    -- Perform XHR requests..
    let toRequest = postJson (rootURL <> "/viewTables") . API.to
    responseEvent <- performRequestAsync $ toRequest <$> updated tableKeysDyn
    -- ..and decode responses.
    let apiTables :: Event t (API.Tables)
        apiTables = mapMaybe id $ decodeXhrResponse <$> responseEvent
    logEvent apiTables $ \t -> "DOM-viewTables: " ++ show t
    holdDyn Map.empty $ API.from <$> apiTables

  setRows rowsDyn = do
    let toRequest = postJson (rootURL <> "/setRows") . API.to
    void $ performRequestAsync $ toRequest <$> updated rowsDyn
    logEvent (updated rowsDyn) $ \t -> "DOM-setRows: " ++ show t

  setTables tablesDyn = do
    let toRequest = postJson (rootURL <> "/setTables") . API.to
    void $ performRequestAsync $ toRequest <$> updated tablesDyn
    logEvent (updated tablesDyn) $ \t -> "DOM-setTables: " ++ show t

  rmRows rowsIndexDyn = do
    let toRequest = postJson (rootURL <> "/rmRows") . API.to
    void $ performRequestAsync $ toRequest <$> updated rowsIndexDyn
    logEvent (updated rowsIndexDyn) $ \t -> "DOM-rmRows: " ++ show t

  rmTables tableKeysDyn = do
    let toRequest = postJson (rootURL <> "/rmTables") . API.to
    void $ performRequestAsync $ toRequest <$> updated tableKeysDyn
    logEvent (updated tableKeysDyn) $ \t -> "DOM-rmTables: " ++ show t

  updateable = pure True

  -- TODO: \o c -> o
  update original changes = pure $ zipDynWith (\o c -> o) original changes

  watchRow keysF = do
    logEvent (updated $ snd <$> keysF) $ \r -> "WS row: " ++ show r
    let messageF = updated $ (\(tk, rk) -> [pack $ show tk ++ show rk]) <$> keysF
    logEvent (messageF) $ \m -> "WS message: " ++ show m
    ws <- webSocket "ws://localhost:3002/watch" $ def
      & webSocketConfig_send .~ messageF
    logEvent (_webSocket_recv ws) $ \r -> "WS received: " ++ show r
    pure $ pure Nothing

-- | Logs a string to the console when an event fires.
--
-- Differs slightly from 'Reflex.traceEvent' because it will print even if the
-- event is otherwise unused. from the reflex-dom-contrib package.
logEvent :: MonadWidget t m => Event t a -> (a -> String) -> m ()
logEvent e mkStr = performEvent_ (liftIO . putStrLn . mkStr <$> e)
