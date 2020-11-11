{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE UndecidableInstances  #-}

module Telescope.DS.Reflex.Dom where

import           Control.Lens
import           Control.Monad          ( void )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Map              as Map
import           Data.Text              ( Text, pack, unpack )
import           Reflex.Dom
import           Telescope.Server.API  as API
import           Telescope.Table       as Table
import           Telescope.Store       as Store
import           Telescope.Class        ( Telescope(..), ToFromF(..) )

rootURL :: Text
rootURL = "http://localhost:3002"

instance
  ( Functor m
  , Applicative m
  , Monad m
  , MonadWidget t m
  , Applicative (Dynamic t)
  , Reflex t
  ) => Telescope m (Dynamic t) where

  viewTables tableKeysDyn = do
    -- Perform XHR requests..
    let toRequest = postJson (rootURL <> "/viewTables") . API.toAPITableKeys
    responseEvent <- performRequestAsync $ toRequest <$> updated tableKeysDyn
    -- ..and decode responses.
    let apiTables :: Event t (API.Tables)
        apiTables = mapMaybe id $ decodeXhrResponse <$> responseEvent
    logEvent apiTables $ \t -> "\nviewTables: " ++ show t
    holdDyn Map.empty $ API.fromAPITables <$> apiTables

  setRows rowsDyn = do
    let toRequest = postJson (rootURL <> "/setRows") . API.toAPITables
    performRequestAsync $ toRequest <$> updated rowsDyn
    logEvent (updated rowsDyn) $ \t -> "setRows: " ++ show t

  setTables tablesDyn = do
    let toRequest = postJson (rootURL <> "/setTables") . API.toAPITables
    performRequestAsync $ toRequest <$> updated tablesDyn
    logEvent (updated tablesDyn) $ \t -> "setTables: " ++ show t

  --  -- dyn :: Dynamic t (m a) -> m (Event t a)
  -- perform aMDyn = do
  --   -- Event of set actions.
  --   let aMEvn = updated aMDyn
  --   logEvent (const "About to setRow" <$> aMEvn) show
  --   widgetHold_ (pure ()) aMEvn
  --   -- dyn aMDyn
  --   -- performEvent_ $ (PerformEventT) <$> aMEvn

-- | Prints a string when an event fires.
--
-- Differs slightly from 'Reflex.traceEvent' because it will print even if the
-- event is otherwise unused. Copied from the reflex-dom-contrib package.
logEvent :: MonadWidget t m => Event t a -> (a -> String) -> m ()
logEvent e mkStr = performEvent_ (liftIO . putStrLn . mkStr <$> e)
