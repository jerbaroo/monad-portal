{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Telescope.DS.Reflex.Dom where

import           Control.Bool                ( guard' )
import qualified Control.Concurrent.MVar    as MVar
import           Control.Monad               ( void )
import           Control.Monad.IO.Class      ( MonadIO, liftIO )
import qualified Data.Map                   as Map
import           Data.Text                   ( Text, pack, unpack )
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock            as Clock
import           Reflex.Dom
import qualified Telescope.Server.API.Types as API
import           Telescope.Class             ( Telescope(..) )

-- | Root HTTP URL of the web server.
-- TODO: Make an argument of a 'run' function. Issue #21.
rootURL :: Text
rootURL = "http://localhost:3002"

-- | Root WebSocket URL of the web server.
-- TODO: Make an argument of a 'run' function. Issue #21.
wsRootURL :: Text
wsRootURL = "ws://localhost:3002"

-- * Reflex-DOM 'Telescope' instance.
--
-- $telescopeInstance
--
-- An instance of the 'Telescope' typeclass for use within a Reflex-DOM
-- application. Uses HTTP and WebSockets to communicate with a Telescope server.
-- The Telescope server will act as a proxy to a database, allowing a Reflex-Dom
-- application to transparently communicate with the database.

instance (Functor m, MonadWidget t m, Reflex t) => Telescope m (Event t) where

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
    ws <- webSocket (wsRootURL <> "/watchRow")$ def & webSocketConfig_send .~
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
    ws <- webSocket (wsRootURL <> "/watchTable") $ def & webSocketConfig_send .~
      ((\tk -> [pack $ show tk]) <$> tableKeyEvn)
    let responses = read . unpack . Text.decodeUtf8 <$> _webSocket_recv ws
    -- And filter out any out-of-date responses..
    tableKeyMayDyn <- current <$> holdDyn Nothing (Just <$> tableKeyEvn)
    pure $ snd <$> attachWithMaybe
      (\tableKeyMay response -> maybe (Just response)
        (\tableKey -> guard' (tableKey == fst response) response) tableKeyMay)
      tableKeyMayDyn responses

-- * Helper Functions
--
-- $helperFunctions
--
-- Functions found to be useful when developing a Reflex-DOM application.

-- | Filter out viewed values that have been recently set.
--
-- This can help avoid recursively viewing and setting values.
filterSetView :: (Ord a, Show a, MonadIO (PushM t), MonadWidget t m)
  => Integer -> Event t a -> Event t (Maybe a) -> m (Event t (Maybe a))
filterSetView maxTime setEvn viewMayEvn = do
  setTimeMapMVar <- liftIO $ MVar.newMVar Map.empty
  -- Record the time of the most recent set event for each 'a'.
  let recordTime a = liftIO $ MVar.modifyMVar_ setTimeMapMVar $ \setTimeMap ->
        pure . flip (Map.insert a) setTimeMap =<< Clock.getCurrentTime
  void $ performEvent $ recordTime <$> setEvn
  -- Choose between newly viewed value and previously viewed value.
  -- TODO: Shouldn't need foldDyn if we are just ignoring second argument.
  let choose Nothing        _ = liftIO $ pure $ Just Nothing
      choose (Just viewedA) _ = liftIO $ do
        drop' <- MVar.modifyMVar setTimeMapMVar $ \setTimeMap -> do
          currTime <- Clock.getCurrentTime
          case Map.lookup viewedA setTimeMap of
            Nothing      -> pure (setTimeMap, False)
            Just setTime -> do
              let diffTime = Clock.utctDayTime currTime - Clock.utctDayTime setTime
              -- putStrLn $ "diff time = " ++ show diffTime ++ " for " ++ show viewedA
              if   diffTime > Clock.secondsToDiffTime maxTime
              then do
                pure (setTimeMap, False)
              else pure (setTimeMap, True)
        -- putStrLn $ "DROPPING = " ++ show drop'
        pure $ if drop' then Nothing else Just $ Just viewedA
  updated <$> foldDynMaybeM choose Nothing viewMayEvn

-- | Logs a string to the console when an event fires.
--
-- Differs slightly from 'Reflex.traceEvent' because it will print even if the
-- event is otherwise unused. Copied from the reflex-dom-contrib package.
logEvent :: MonadWidget t m => Event t a -> (a -> String) -> m ()
logEvent e mkStr = performEvent_ $ liftIO . putStrLn . mkStr <$> e
