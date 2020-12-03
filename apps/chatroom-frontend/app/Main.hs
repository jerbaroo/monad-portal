{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

import           Control.Lens             ( (^.) )
import           Data.Proxy               ( Proxy(..) )
import           Reflex.Dom
import           Safe.Foldable            ( maximumMay )
import           Telescope.DS.Reflex.Dom  ()
import qualified Telescope.Operations    as T
import           ChatRoom.Common          ( Message(..) )

main :: IO ()
main = mainWidget $ do
  -- A text field to enter chat room name and username.
  (roomNameDyn, usernameDyn) <- do
    roomNameInput <- textInputPlaceholder "Chat Room"
    usernameInput <- textInputPlaceholder "Username"
    pure (roomNameInput ^. textInput_value, usernameInput ^. textInput_value)
  -- View all messages live from the database.
  dbMessagesEvn <- T.viewTableRx $ const (Proxy @Message) <$> updated roomNameDyn
  -- Filter messages to the current chat room.
  roomMessagesDyn <- holdDyn [] $ attachPromptlyDynWith
    (\rn ms -> [m | m <- ms, room m == rn]) roomNameDyn dbMessagesEvn
  let nextIdDyn = (1 +) . maybe 0 id . maximumMay . map mId <$> roomMessagesDyn
  -- Display messages for the current chat room.
  _ <- el "ul" $ simpleList roomMessagesDyn $ el "li" . dynText . fmap
    (\m -> "“" <> username m <> "”: " <> message m)
  -- A text field for entering messages, and button to send the message.
  messageTextDyn <- (^. textInput_value) <$> textInputPlaceholder "Enter Message"
  sendEvn        <- button "Send"
  -- Construct a 'Message' from user input, and send on button click.
  let messageToSendDyn = (\room username message mId -> Message {..})
        <$> roomNameDyn <*> usernameDyn <*> messageTextDyn <*> nextIdDyn
      messageToSendEvn = tagPromptlyDyn messageToSendDyn sendEvn
  T.setRx messageToSendEvn
  -- Factor out text input field construction.
  where textInputPlaceholder placeholder = textInput $ def
          & textInputConfig_attributes .~ pure ("placeholder" =: placeholder)
