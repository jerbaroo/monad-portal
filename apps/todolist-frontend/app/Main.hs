{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Bool             ( bool, guard' )
import           Control.Lens             ( (^.) )
import           Control.Monad            ( join )
import           Data.Function.Flip       ( flip3 )
import qualified Data.Map                as Map
import           Data.Maybe               ( catMaybes, isNothing )
import           Data.Text                ( Text )
import           Data.Tuple.Extra         ( dupe, fst3, snd3, thd3 )
import           Data.Generics.Labels     ()
import           Reflex.Dom
import           Telescope.DS.Reflex.Dom  ( filterSetView )
import qualified Telescope.Operations    as T
import           ToDoList.Common          ( ToDoList(..) )

main :: IO ()
main = mainWidget $ el "table" $ mdo
  -- A text field to enter list name, and a button to add a list item.
  (nameDyn, addRowEvn) <- el "tr" $ do
    nameDyn'   <- el "td" $ (^. textInput_value) <$> textInput (def
      & textInputConfig_attributes .~ (pure $ "placeholder" =: "List Name"))
    addRowEvn' <- el "td" $ disabledButton ((==) "" <$> nameDyn) "Add Item"
    pure (nameDyn', addRowEvn')
  -- View the to-do list "live" from the database. Filter with 'filterSetView'
  -- to avoid recursively viewing lists that have just been set (within 1s).
  dbListMayEvn <- T.viewKRx $ updated nameDyn
  filteredListMayEvn <- filterSetView 1 listToSetEvn dbListMayEvn
  -- Convert 'Maybe ToDoList' to 'ToDoList'. If there is 'Nothing' in the
  -- database, promptly create an empty list using the name in 'nameDyn'.
  filteredListDyn <- holdDyn (ToDoList "" []) $ attachPromptlyDynWith
    (\n -> maybe (ToDoList n []) id) nameDyn filteredListMayEvn
  -- Add an empty item when "Add Item" is clicked. And restructure into '(Text,
  -- [(Maybe Text, Maybe Text)])': list name, and pairs of database/new value.
  numAddedDyn <- foldDyn (\aMay n -> maybe 0 (+ n) aMay) 0 $ leftmost
    [const Nothing <$> updated filteredListDyn, const (Just 1) <$> addRowEvn]
  let itemTuplesDyn' = (\num l -> do
        let db  = uncurry (,) . dupe . Just <$> l ^. #items
            new = replicate num (Nothing, Just "") -- Added rows are empty.
        (name l, db ++ new)) <$> numAddedDyn <*> filteredListDyn
  -- Pad the list with (name, Nothing, Nothing) to hide any removed items.
  let combine (n, dbNew) curr = map (\(db, new) -> (n, db, new)) dbNew
        ++ (replicate (length curr - length dbNew) (n, Nothing, Nothing))
  itemTuplesDyn <- flip3 foldDyn (updated itemTuplesDyn') [] combine
  -- Collect user input for each list item/table row.
  tableRowsDyn <- simpleList itemTuplesDyn $ \givenTupleDyn -> mdo
    initialGivenTuple <- sample $ current givenTupleDyn
    -- Item is displayed/hidden based on: given input, when item is removed.
    hiddenAttrDyn <- fmap (bool Map.empty ("hidden" =: "")) <$> holdDyn False (
      leftmost [isNothing . thd3 <$> updated givenTupleDyn, const True <$> removeEvn])
    -- The HTML table row: a text input and remove button.
    (textEnteredDyn, removeEvn) <- elDynAttr "tr" hiddenAttrDyn $ mdo
      itemInput <- el "td" $ textInput $ def
        & textInputConfig_initialValue .~ maybe "" id (thd3 initialGivenTuple)
        & textInputConfig_setValue     .~ fmapMaybe id (thd3 <$> updated givenTupleDyn)
      removeEvn' <- el "td" $ button "X"
      pure (itemInput ^. textInput_value, removeEvn')
    -- Text is updated from: given input, user input, or when item is removed.
    let newTextMayEvn = leftmost
          [ thd3 <$> updated givenTupleDyn
          , Just <$> updated textEnteredDyn
          , const Nothing <$> removeEvn ]
    holdDyn initialGivenTuple $ attachPromptlyDynWith
      (\(a,b,_) -> (a,b,)) givenTupleDyn newTextMayEvn
  -- Filter user output, keeping only data to set in the database.
  let tableRowsEvn = updated $ join $ sequence <$> tableRowsDyn
      filterEvn    = ffilter (\l -> length l > 0 && (map snd3 l /= map thd3 l)) tableRowsEvn
      listToSetEvn = (\l -> ToDoList (fst3 $ head l) (catMaybes $ map thd3 l)) <$> filterEvn
  T.setRx listToSetEvn

-- | A button with dynamic "disabled" attribute and text.
disabledButton :: MonadWidget t m => Dynamic t Bool -> Dynamic t Text -> m (Event t ())
disabledButton disabledDyn textDyn = do
  let attrs b = Map.fromList $ [("type", "button")] ++ guard' b ("disabled", "")
  domEvent Click . fst <$> elDynAttr' "button" (attrs <$> disabledDyn) (dynText textDyn)
