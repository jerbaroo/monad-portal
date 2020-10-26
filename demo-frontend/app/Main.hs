{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecursiveDo          #-}

import           Demo.Common              ( Person(..) )
import           Data.Text.Encoding       ( encodeUtf8, decodeUtf8 )
import           Data.Text                ( pack, unpack )
import           Control.Lens             ( (^.), view )
import           Reflex.Dom
import qualified Telescope.Ops           as T
import qualified Telescope.Class         as Class
import           Telescope.DS.Reflex.Dom  ( logEvent, root )
import qualified Telescope.Table         as Table

main = mainWidget $ el "div" $ do
  el "h3" $ text "viewTableRows"
  el "p" $ text "enter table name:"
  viewTableRowsInputDyn <- textInput def
  table <- Class.viewTableRows $
    Table.TableKey . unpack <$> viewTableRowsInputDyn ^. textInput_value
  dynText $ fmap (pack . show) table

  el "h3" $ text "viewTableRx"
  viewTableButtonEvent <- button "click to view Person table"
  people <- T.viewTableRx
    =<< holdDyn Person{} (const Person{} <$> viewTableButtonEvent)
  dynText $ fmap (pack . show) people

  el "h3" $ text "viewKRx"
  el "p" $ text "Enter Person's name:"
  nameInputDyn <- textInput def
  people <- flip T.viewKRx
    (unpack <$> nameInputDyn ^. textInput_value)
    (const Person{} <$> (nameInputDyn ^. textInput_value))
  dynText $ fmap (pack . show) people

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
