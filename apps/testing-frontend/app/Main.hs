{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}

import           Control.Lens             ( (^.), view )
import           Control.Monad            ( void, when )
import           Data.Either              ( isLeft )
import           Data.Either.Combinators  ( rightToMaybe )
import           Data.Functor             ( (<&>) )
import qualified Data.Map                as Map
import           Data.Proxy               ( Proxy(Proxy) )
import           Data.Text                ( Text, pack, unpack )
import           Data.These               ( These(..) )
import           Reflex.Dom
import qualified Telescope               as T
import           Telescope.DS.Reflex.Dom  ()
import           Testing.Common           ( Person(..) )
import           Text.Read                ( readMaybe )

-- | Initial value's for input fields.
initPerson :: Person
initPerson = Person "John" 21

-- | Widget's to test different server endpoints.
main :: IO ()
main = mainWidget $ el "div" $ do
  viewWidget
  viewKWidget
  viewTableWidget
  setWidget
  setTableWidget
  rmWidget
  rmKWidget
  rmTableWidget

-- | 'T.viewRx' a Person with user-input name.
viewWidget :: MonadWidget t m => m ()
viewWidget = do
  el "h3" $ text "viewRx"
  nameDyn   <- view textInput_value <$> textInput (def &
    textInputConfig_attributes .~ (pure $ "placeholder" =: "Person's name"))
  personEvn <- T.viewRx $ updated $ (\name -> Person{..}) <$> nameDyn
  dynText =<< holdDyn "Waiting on input" (pack . (" " ++) . show <$> personEvn)

-- | 'T.viewKRx' a Person with user-input name.
viewKWidget :: MonadWidget t m => m ()
viewKWidget = do
  el "h3" $ text "viewKRx"
  nameDyn   <- view textInput_value <$> textInput (def &
    textInputConfig_attributes .~ (pure $ "placeholder" =: "Person's name"))
  personEvn <- T.viewKRx @Person $ updated nameDyn
  dynText =<< holdDyn "Waiting on input" (pack . (" " ++) . show <$> personEvn)

-- | 'T.viewTableRx' the Person table.
viewTableWidget :: MonadWidget t m => m ()
viewTableWidget = do
  el "h3" $ text "viewTableRx"
  peopleEvn <- T.viewTableRx . fmap (const (Proxy @Person)) =<< now
  void . dyn . fmap personTable =<< holdDyn [] peopleEvn

-- | 'T.setRx' a Person.
setWidget :: MonadWidget t m => m ()
setWidget = do
  el "h3" $ text "setRx"
  -- User input, converted to a 'Dynamic t Person'.
  nameInput <- textInput $ def & textInputConfig_initialValue .~ name initPerson
  ageEiDyn  <- numberInput (pack $ show $ age initPerson) parseAge
  let personEiDyn = zipDynWith (\name ageEi -> ageEi <&> (\age -> Person {..}))
        (nameInput ^. textInput_value) ageEiDyn
  -- Set 'Person' on every click 'Event' (filtered to valid input).
  clickEvn <- disabledButton (isLeft <$> personEiDyn) "Set Person"
  T.setRx $ attachPromptlyDynWithMaybe (const . rightToMaybe) personEiDyn clickEvn
  dynText =<< (holdDyn "" $ errText <$> updated ageEiDyn)

-- | 'T.setTableRx' a table of Person.
setTableWidget :: MonadWidget t m => m ()
setTableWidget = do
  el "h3" $ text "setTableRx"
  let minRows = 1
      countRows (This _)    n = max minRows (n - 1) -- Don't go below 'minRows'.
      countRows (That _)    n = n + 1
      countRows (These _ _) n = n -- Both buttons clicked at same time.
  -- Buttons above the input table.
  rec countPEvn  <- button "+ Row"
      countNEvn  <- disabledButton ((<= 1) <$> numRowsDyn) "- Row"
      setEvn     <- disabledButton (isLeft <$> peopleEiDyn) "Set Table"
      numRowsDyn <- foldDyn countRows minRows $
        alignEventWithMaybe Just countNEvn countPEvn
      -- The input table.
      -- TODO: factor out into 'personTable'.
      inputTableDyn <- el "table" $ do
        el "tr" $ (el "td" $ text "Name") >> (el "td" $ text "Age")
        simpleList ((\a -> [(1::Int)..a]) <$> numRowsDyn) $ const $ el "tr" $ do
          nameInput <- el "td" $ textInput $ def & textInputConfig_initialValue .~ name initPerson
          ageEiDyn  <- el "td" $ numberInput (pack $ show $ age initPerson) parseAge
          dynText =<< (holdDyn "" $ errText <$> updated ageEiDyn)
          pure $ (\name -> fmap (\age -> (name, age)))
            <$> nameInput ^. textInput_value <*> ageEiDyn
      -- Set input table on click.
      let inputEiDyn  = distributeListOverDynWith sequence =<< inputTableDyn
          peopleEiDyn = (fmap $ map $ \(name, age) -> Person{..}) <$> inputEiDyn
  peopleBhv <- hold [] $ filterRight $ (updated peopleEiDyn)
  T.setTableRx $ tag peopleBhv setEvn

-- | 'T.rmRx' a Person with user-input name.
rmWidget :: MonadWidget t m => m ()
rmWidget = do
  el "h3" $ text "rmRx"
  let toPerson = fmap (\name -> Person{..}) . view textInput_value
  personDyn <- toPerson <$> textInput (def &
    textInputConfig_attributes .~ (pure $ "placeholder" =: "Person's name"))
  clickEvn  <- button "Remove"
  T.rmRx $ tag (current personDyn) clickEvn

-- | 'T.viewKRx' a Person with user-input name.
rmKWidget :: MonadWidget t m => m ()
rmKWidget = do
  el "h3" $ text "rmKRx"
  nameDyn  <- view textInput_value <$> textInput (def &
    textInputConfig_attributes .~ (pure $ "placeholder" =: "Person's name"))
  clickEvn <- button "Remove"
  T.rmKRx @Person $ tag (current nameDyn) clickEvn

-- | 'T.rmTableRx' the Person table.
rmTableWidget :: MonadWidget t m => m ()
rmTableWidget = do
  el "h3" $ text "rmTableRx"
  clickEvn <- button "Remove"
  T.rmTableRx (const (Proxy @Person) <$> clickEvn)

----------------------
-- HELPER FUNCTIONS --
----------------------

-- | A list of 'Person' to a HTML "table".
personTable :: DomBuilder t m => [Person] -> m ()
personTable people = el "table" $ do
  when (not $ null people) $ do
    el "td" $ text "Name"
    el "td" $ text "Age"
  mapM_ toTableRow people
  where toTableRow :: DomBuilder t m => Person -> m ()
        toTableRow person = el "tr" $ do
          td $ name person
          td $ pack $ show $ age person
        td :: DomBuilder t m => Text -> m ()
        td text =
          void $ el "td" $ inputElement $ def
            & inputElementConfig_initialValue .~ text
            & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
              ("disabled" =: "true")

-- | Error text with a leading space, or empty text.
errText :: Either Text a -> Text
errText (Left t) = " " <> t
errText _        = ""

-- | A valid age parsed from text, or an error message.
parseAge :: Text -> Either Text Int
parseAge t =
  let min = 0; max = 150;
  in case readMaybe $ unpack t of
    Nothing  -> Left $ "Couldn't parse \"" <> t <> "\" as \"Int\""
    Just int ->
      case int < min || int > max of
        True  -> Left $ "Not in range [" <> (pack $ show min) <> " " <> (pack $ show max) <> "]"
        False -> Right int

-- | A number input that returns parsed input.
numberInput :: DomBuilder t m => Text -> (Text -> Either a b) -> m (Dynamic t (Either a b))
numberInput initial parse = do
  n <- inputElement $ def
    & inputElementConfig_initialValue .~ initial
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
      ("type" =: "number")
  return $ fmap parse $ _inputElement_value n

-- | A button that may be disabled.
disabledButton :: MonadWidget t m => Dynamic t Bool -> Text -> m (Event t ())
disabledButton disabledDyn s = do
  (e, _) <- flip (elDynAttr' "button") (text s) $
    (\b -> Map.fromList $ [("type", "button")] ++ disabledAttr b)
    <$> disabledDyn
  return $ domEvent Click e
  where disabledAttr True = [("disabled", "")]
        disabledAttr _    = []
