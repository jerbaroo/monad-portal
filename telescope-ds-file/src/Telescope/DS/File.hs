{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Telescope.DS.File where

import           Control.Comonad          ( extract )
import qualified Control.Concurrent.MVar as MVar
import           Control.Exception        ( catch, throwIO )
import           Control.Monad            ( forM_, void, when )
import           Control.Monad.IO.Class   ( MonadIO, liftIO )
import qualified Data.ByteString         as BS
import           Data.Either.Extra        ( fromRight' )
import           Data.Functor.Identity    ( Identity(..) )
import           Data.List                ( nub )
import qualified Data.Map                as Map
import qualified Data.Set                as Set
import           Data.Maybe               ( catMaybes, fromJust, isJust)
import           Flat                     ( Flat )
import qualified Flat                    as Flat
import           System.Directory         ( canonicalizePath )
import           System.FilePath          ( takeDirectory )
import           System.FSNotify          ( Event (Modified) )
import qualified System.FSNotify         as FS
import           System.IO.Error          ( isDoesNotExistError )
import           Telescope.Class          ( Telescope )
import qualified Telescope.Class         as Class
import qualified Telescope.Table.Types   as Table

--------------------------------------------------------------------------------
-- Telescope instance and runT -------------------------------------------------
--------------------------------------------------------------------------------

-- | A naive file-backed data source, functional but slow.
newtype TFile a = TFile (IO a) deriving (Functor, Applicative, Monad, MonadIO)

runT :: MonadIO m => TFile a -> m a
runT (TFile a) = liftIO a

instance Telescope TFile Identity where

  viewRows = Class.viewRowsCheap

  viewTables tableKeysSetF = do
    let tableKeysF = Set.toList <$> tableKeysSetF -- Order for zipping.
    tables <- fmap toNormalTable <$> (mapM readTableOnDisk $ extract tableKeysF)
    pure $ pure $ Map.fromList $ zip (extract tableKeysF) tables

  setRows = Class.setRowsCheap

  setTables tablesF = do
    let keysTableKeysF = Map.toList $ extract tablesF -- Order for zipping.
    tablesOnDisk <- mapM readTableOnDisk $ map fst keysTableKeysF
    forM_ (zip tablesOnDisk $ keysTableKeysF) $
      \(tableOnDisk, (tableKey, table)) ->
        liftIO $ BS.writeFile (tablePath tableKey) $ Flat.flat $
          updatedTableOnDisk table tableOnDisk

  rmRows = Class.rmRowsCheap

  updateable = pure False

  onChangeRow keysF fF = do
    onChangeRow' (fst $ extract keysF) (snd $ extract keysF) (extract fF)

  onChangeTable tableKeyF fF =
    onChangeTable' (extract tableKeyF) (extract fF . toNormalTable)

--------------------------------------------------------------------------------
-- TableOnDisk type and Helper Functions ---------------------------------------
--------------------------------------------------------------------------------

-- | The current value (if exists) and update count for each row in a table.
type TableOnDisk = Map.Map Table.RowKey (Maybe Table.Row, Int)

-- | Convert from TFile-specific format to 'Table.Table' format.
toNormalTable :: TableOnDisk -> Table.Table
toNormalTable = fmap (fromJust . fst) . Map.filter (isJust . fst)

-- | File path for a table.
tablePath :: Table.TableKey -> String
tablePath (Table.TableKey tkName) = tkName ++ ".table"

-- | Read a table from disk.
readTableOnDisk :: Table.TableKey -> TFile TableOnDisk
readTableOnDisk tableKey = liftIO $ readOrDefault Map.empty $ tablePath tableKey

-- | Decode a value of type 'a' from a file, or return a default value.
--
-- TODO: replace fromRight' with fromRight and raise DecodeError.
--
-- In-case of a file-not-found error the default value is returned.
readOrDefault :: Flat a => a -> FilePath -> IO a
readOrDefault default' path = do
  liftIO . print =<< canonicalizePath path
  flip catch catchDoesNotExistError $ do
    fromRight' . Flat.unflat <$> BS.readFile path
  where catchDoesNotExistError e
          | isDoesNotExistError e = pure default'
          | otherwise             = throwIO e

---------------------------------------
-- Functions Doing the Heavy Lifting --
---------------------------------------

-- | Apply an updated table with the existing table on disk.
updatedTableOnDisk :: Table.Table -> TableOnDisk -> TableOnDisk
updatedTableOnDisk table onDisk = do
  let keys     = nub $ Map.keys table ++ Map.keys onDisk :: [Table.RowKey]
      lefts    = map (flip Map.lookup table ) keys       :: [Maybe Table.Row]
      rights   = map (flip Map.lookup onDisk) keys       :: [Maybe (Maybe Table.Row, Int)]
      combined = [combine l r | (l, r) <- zip lefts rights]
  Map.fromList $ zip keys $ catMaybes combined

-- | Combine a table row with an existing row on disk.
combine
  :: Maybe Table.Row
  -> Maybe (Maybe Table.Row, Int)
  -> Maybe (Maybe Table.Row, Int)
-- Row to save, row also on disk.
combine (Just row) (Just (Just oldRow, ident))
  | row == oldRow                          = Just (Just row, ident    )
  | otherwise                              = Just (Just row, ident + 1)
-- Row to save, row not on disk anymore.
combine (Just row) (Just (Nothing, ident)) = Just (Just row, ident + 1)
-- Row to save, row not on disk.
combine (Just row) Nothing                 = Just (Just row, 0        )
-- Remove the old value!
combine Nothing    (Just (Just _, ident))  = Just (Nothing, ident + 1 )
-- No row to save, row not on disk anymore.
combine Nothing    (Just (Nothing, ident)) = Just (Nothing, ident     )
-- No row to save, row not on disk.
combine Nothing    Nothing                 = Nothing

-- | Run a function when a value on disk has changed.
onChangeRow'
  :: Table.TableKey -> Table.RowKey -> (Maybe Table.Row -> TFile ()) -> TFile ()
onChangeRow' tableKey rowKey f = liftIO $ do
  -- MVar with row's last update ID.
  tableOnDisk <- runT $ readTableOnDisk tableKey
  let idEntry = Map.lookup rowKey tableOnDisk :: Maybe (Maybe Table.Row, Int)
  lastIdMVar <- MVar.newMVar $ maybe (-1) snd idEntry
  -- Watch the file for changes.
  path <- canonicalizePath $ tablePath tableKey
  let moddedFile (Modified moddedPath _ _) = path == moddedPath
      moddedFile _                         = False
  manager     <- FS.startManager
  void $ FS.watchDir manager (takeDirectory path) moddedFile $ const $ do
    -- Each time the file has changed check for updates.
    runT $ do
      newTableOnDisk <- readTableOnDisk tableKey
      case Map.lookup rowKey newTableOnDisk of
        Nothing                -> pure () -- Row not saved on disk.
        Just (maybeRow, newId) -> do
          -- Check ID of last watched update..
          lastId <- liftIO $ MVar.swapMVar lastIdMVar newId
          -- ..if an update has occured since, run the function.
          when (newId > lastId) (f maybeRow)

-- | Run a function when a value on disk has changed.
onChangeTable' :: Table.TableKey -> (TableOnDisk -> TFile ()) -> TFile ()
onChangeTable' tableKey f = liftIO $ do
  -- MVar with last table.
  tableOnDisk         <- runT $ readTableOnDisk tableKey
  lastTableOnDiskMVar <- MVar.newMVar tableOnDisk
  -- Watch the file for changes.
  path <- canonicalizePath $ tablePath tableKey
  let moddedFile (Modified moddedPath _ _) = path == moddedPath
      moddedFile _                         = False
  manager     <- FS.startManager
  -- Each time the file has changed check for updates.
  void $ FS.watchDir manager (takeDirectory path) moddedFile $ const $ do
    runT $ do
      newTableOnDisk  <- readTableOnDisk tableKey
      lastTableOnDisk <- liftIO $ MVar.swapMVar lastTableOnDiskMVar newTableOnDisk
      when (Map.toList newTableOnDisk /= Map.toList lastTableOnDisk) $ do
        f newTableOnDisk
