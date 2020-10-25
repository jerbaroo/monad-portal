{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE UndecidableInstances  #-}

module Telescope.DS.Reflex.Dom where

import           Control.Arrow    ( (***) )
import           Control.Lens
import qualified Data.Map        as Map
import           Data.Text        ( pack, unpack )
import           Reflex.Dom
import           Telescope.Table as Table
import           Telescope.Class  ( Telescope(..), ToFromF(..) )

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
    -- Perform XHR requests.
    let viewTableUrl tableKey =
          root <> "/viewTable/" <> (pack $ Table.unTableKey tableKey)
        toRequest tableKey    = XhrRequest "GET" (viewTableUrl tableKey) def
    responseEvent <- performRequestAsync $ toRequest <$> updated tableKeyDyn
    -- Convert responses.
    let stringEvent   = fmap (read . unpack) $
          mapMaybe id $ view xhrResponse_responseText <$> responseEvent
        decode (a, b) = (read a, read b) :: (Table.RowKey, Table.Row)
        tableEvent    = (Map.fromList . fmap decode) <$> stringEvent
        -- mergedTables = (Map.fromList . fmap (mapPair read)) <$> textEvent
    holdDyn Map.empty tableEvent

-- TODO: remove.
toRequest query = XhrRequest "GET" (root <> "/viewTable/" <> query) def
