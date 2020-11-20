{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import           Data.Text                ( pack )
import           Control.Lens             ( view )
import           Reflex.Dom
import qualified Telescope.Operations    as T
import           Telescope.DS.Reflex.Dom  ()
import           ToDoList.Common          ( ToDoList(..) )

-- | Widget's to test different server endpoints.
main :: IO ()
main = mainWidget $ el "div" $ do
  nameDyn        <- view textInput_value <$> textInput (def &
    textInputConfig_attributes .~ (pure $ "placeholder" =: "List name"))
  toDoListEvn    <- T.viewRx $ (\name -> ToDoList{..}) <$> updated nameDyn
  toDoListMayDyn <- holdDyn Nothing $ Just <$> toDoListEvn
  dynText $ (maybe "" (pack . show)) <$> toDoListMayDyn
