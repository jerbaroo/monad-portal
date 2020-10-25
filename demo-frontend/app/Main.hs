{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo          #-}

import           Control.Arrow       ( (***) )
import           Demo.Common         ( Person(..) )
import           Data.Text.Encoding  ( encodeUtf8, decodeUtf8 )
import           Data.Text           ( pack, unpack )
import           Control.Lens
import           Reflex.Dom
import           Telescope.Table    as Table
import           Telescope.Server.API as API

import qualified Data.Map           as Map
import           Telescope.Class     ( Telescope(..), ToFromF(..) )

mapPair f = f *** f

instance Reflex t => ToFromF (Dynamic t) a where
  toF = constDyn

root = "http://localhost:3002"

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
    responseEvnt <- performRequestAsync $ toRequest <$> updated tableKeyDyn
    -- Convert responses.
    let stringEvnt    = fmap (read . unpack) $
          mapMaybe id $ view xhrResponse_responseText <$> responseEvnt
        decode (a, b) = (read a, read b) :: (Table.RowKey, Table.Row)
        tableEvnt     = (Map.fromList . fmap decode) <$> stringEvnt
        -- mergedTables = (Map.fromList . fmap (mapPair read)) <$> textEvnt
    holdDyn Map.empty tableEvnt

search queries = do
  let toRequest query = XhrRequest "GET" (root <> "/viewTable/" <> query) def
  -- Async responses to search query.
  responses <- performRequestAsync $ toRequest <$> queries
  -- Extract text from response.
  return $ view xhrResponse_responseText <$> responses

main = mainWidget $ el "div" $ do
  el "p" $ text "Reflex is"
  el "ul" $ do
    el "li" $ text "Efficient"
    el "li" $ text "Higher-order"
    el "li" $ text "Glitch-free"

  el "h3" $ text "Search database"
  -- A text input element.
  input <- textInput def
  -- Results of search query.
  results <- search $ updated $ input ^. textInput_value
  -- Display search results.
  dynText =<< (holdDyn "No results." $ pack . show <$> results)

  table <- viewTableRows =<< (holdDyn (TableKey "") $ (Table.TableKey . show) <$> results)
  dynText $ fmap (pack . show) table

  el "p" $ text "Watch database"
  rec table  <- inputElement def
      rowKey <- inputElement def
      watch  <- button "Watch"
      let newMessage = fmap ((:[]) . encodeUtf8)
            $ tag (current $ zipDynWith (<>) (value table) (value rowKey))
            $ watch
  ws <- webSocket "ws://localhost:3002/watch" $ def
    & webSocketConfig_send .~ newMessage
  pure ()

--------------------------------
-- DEVELOPMENT CODE BELOW     --
-- WILL BE REFACTORED INTO A  --
-- NEW PACKAGE BEFORE RELEASE --
--------------------------------
