{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

import           ChatRoom.Common         ( Message(..) )
import           Control.Lens            ( (^.) )
import           Control.Monad.IO.Class  ( MonadIO, liftIO )
import           Data.Proxy              ( Proxy(..) )
import           Data.Time.Clock         ( NominalDiffTime )
import           Data.Time.Clock.POSIX   ( getPOSIXTime )
import           Reflex.Dom
import qualified Telescope               as T
import           Telescope.DS.Reflex.Dom  ()

main :: IO ()
main = mainWidget $ do
  -- A text field to enter chat room name and username.
  (roomNameDyn, usernameDyn) <- do
    roomNameInput <- textInputPlaceholder "Chat Room"
    usernameInput <- textInputPlaceholder "Username"
    pure (roomNameInput ^. textInput_value, usernameInput ^. textInput_value)
  -- View messages live from the database.
  dbMessagesEvn <- T.viewTableRx $ const (Proxy @Message) <$> updated roomNameDyn
  -- Filter messages to the current chat room.
  roomMessagesDyn <- holdDyn [] $ attachPromptlyDynWith
    (\rn ms -> [m | m <- ms, room m == rn]) roomNameDyn dbMessagesEvn
  -- Display messages for the current chat room.
  el "ul" $ simpleList roomMessagesDyn $ el "li" . dynText . fmap
    (\m -> "“" <> username m <> "”: " <> message m)
  -- A text field for entering messages, and button to send the message.
  messageTextDyn <- (^. textInput_value) <$> textInputPlaceholder "Enter Message"
  timeEvn        <- fmap (fmap round) . tagTime =<< button "Send"
  -- Construct a 'Message' from user input, and send on button click.
  let messageToSendDyn = (\room username message time -> Message {..})
        <$> roomNameDyn <*> usernameDyn <*> messageTextDyn
      messageToSendEvn = attachPromptlyDynWith ($) messageToSendDyn timeEvn
  T.setRx messageToSendEvn
  -- Data from the additional server integrated with Telescope's server.
  responseEvent <- performRequestAsync . fmap (const $
    XhrRequest "GET" "http://localhost:3002/additional" def) =<< getPostBuild
  dynText =<< (holdDyn "" $ mapMaybe id $ decodeXhrResponse <$> responseEvent)

-- | Factor out text-input construction.
textInputPlaceholder placeholder = textInput $ def
  & textInputConfig_attributes .~ pure ("placeholder" =: placeholder)

-- | Replace each event occurence with the current time.
tagTime :: ( PerformEvent t m, MonadIO (Performable m), Reflex t )
  => Event t a -> m (Event t NominalDiffTime)
tagTime e = performEvent . ffor e $ const $ liftIO getPOSIXTime
