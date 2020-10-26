{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE UndecidableInstances  #-}

module Telescope.DS.Reflex.Dom where

import           Control.Arrow          ( (***) )
import           Control.Lens
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Map              as Map
import           Data.Text              ( pack, unpack )
import           Reflex.Dom
import           Telescope.Server.API  as API
import           Telescope.Table       as Table
import           Telescope.Store       as Store
import           Telescope.Class        ( Telescope(..), ToFromF(..) )

mapPair f = f *** f

root = "http://localhost:3002"

instance Reflex t => ToFromF (Dynamic t) a where
  toF = constDyn

instance (
    Functor m
  , Applicative m
  , Monad m
  , MonadWidget t m
  , Applicative (Dynamic t)
  , Reflex t
  ) => Telescope m (Dynamic t) where
  viewTableRows tableKeyDyn = do
    -- Perform XHR requests..
    let viewTableUrl tableKey =
          root <> "/viewTable/" <> (pack $ Table.unTableKey tableKey)
        toRequest tableKey    = XhrRequest "GET" (viewTableUrl tableKey) def
    responseEvent <- performRequestAsync $ toRequest <$> updated tableKeyDyn
    -- ..and decode responses.
    let tableAsListEvent = mapMaybe id $
          decodeXhrResponse <$> responseEvent :: Event t (API.TableAsList)
    holdDyn Map.empty $ Map.fromList <$> tableAsListEvent

-- | Prints a string when an event fires.  This differs slightly from
-- traceEvent because it will print even if the event is otherwise unused.
logEvent :: MonadWidget t m => Event t a -> (a -> String) -> m ()
logEvent e mkStr = do
    performEvent_ (liftIO . putStrLn . mkStr <$> e)

-- TODO: remove.
toRequest query = XhrRequest "GET" (root <> "/viewTable/" <> query) def
