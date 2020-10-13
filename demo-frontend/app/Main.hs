{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Text    (pack)
import           Control.Lens
import           Reflex.Dom

url query = "http://localhost:3001/viewTable/" <> query

search queries = do
  let toRequest query = XhrRequest "GET" (url query) def
  -- Async responses to search query.
  responses <- performRequestAsync $ toRequest <$> queries
  -- Extract text from response.
  return $ view xhrResponse_responseText <$> responses

main = mainWidget $ el "div" $ do
  -- A text input element.
  input <- textInput def
  -- Text input as an 'Event'.
  let queries = updated $ input ^. textInput_value
  el "p" $ text " "
  -- Results to search query.
  results <- search queries
  -- 'Dynamic' from search results.
  asText <- holdDyn "No results." $ pack . show <$> results
  -- Display search results.
  dynText asText
  el "p" $ text "Reflex is"
  el "ul" $ do
    el "li" $ text "Efficient"
    el "li" $ text "Higher-order"
    el "li" $ text "Glitch-free"

--------------------------------
-- DEVELOPMENT CODE BELOW     --
-- WILL BE REFACTORED INTO A  --
-- NEW PACKAGE BEFORE RELEASE --
--------------------------------
