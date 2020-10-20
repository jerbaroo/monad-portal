{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Telescope.DS.File where

import           Control.Concurrent.MVar as MVar
import           Control.Exception       ( catch, throwIO )
import           Control.Monad           ( void, when )
import           Control.Monad.IO.Class  ( MonadIO, liftIO )
import           Data.ByteString.Char8   ( pack, unpack )
import           Data.Either.Extra       ( fromRight' )
import           Data.Functor.Identity   ( Identity(..) )
import           Data.List               ( nub )
import qualified Data.Map               as Map
import           Data.Maybe              ( catMaybes, fromJust, isJust)
import           Data.Serialize          ( Serialize, decode, encode )
import           System.Directory        ( canonicalizePath )
import           System.FilePath         ( takeDirectory )
import           System.FSNotify         ( Event (Modified) )
import qualified System.FSNotify        as FS
import           System.IO.Error         ( isDoesNotExistError )
import qualified System.IO.Strict       as Strict
import           Telescope.Class         ( Telescope(..), ToFromF(..) )
import qualified Telescope.Table        as Table

-- | A naive file-backed data source, functional but slow.
newtype TFile a = TFile (IO a) deriving (Functor, Applicative, Monad, MonadIO)

-- | Telescope operations are not packed in any special container.
newtype TFileIdentity a = TFileIdentity (Identity a)
  deriving (Functor, Applicative)

-- | Enable conversion to/from the container.
instance ToFromF TFileIdentity a where
  toF = TFileIdentity . Identity
  fromF (TFileIdentity (Identity a)) = a

runT :: MonadIO m => TFile a -> m a
runT (TFile a) = liftIO a

-- | File path for a table.
tablePath :: Table.TableKey -> String
tablePath (Table.TableKey tkName) = tkName ++ ".table"

-- | Read a table from disk.
readTableOnDisk :: Table.TableKey -> TFile TableOnDisk
readTableOnDisk tableKey = liftIO $ readOrDefault Map.empty $ tablePath tableKey

instance Telescope TFile TFileIdentity where

  viewTableRows tableKeyId = do
    tableOnDisk <- readTableOnDisk $ fromF tableKeyId
    pure $ toF $ fmap (fromJust . fst) $ Map.filter (isJust . fst) tableOnDisk

  -- TODO: file lock this function.
  setTableRows tableKeyId tableId = do
    let (tableKey, table) = (fromF tableKeyId, fromF tableId)
    tableOnDisk <- readTableOnDisk tableKey
    liftIO $ writeFile (tablePath tableKey) $ unpack $ encode $
      newUpdates table tableOnDisk
     
  onChangeRow tableKeyId rowKeyId fId =
    onChangeRow' (fromF tableKeyId) (fromF rowKeyId) (fromF fId)

-- | The current value (if exists) and update count for each row in a table.
type TableOnDisk = Map.Map Table.RowKey (Maybe Table.Row, Int)

-- | Combine a table to write to disk with the existing table on disk.
newUpdates :: Table.Table -> TableOnDisk -> TableOnDisk
newUpdates table onDisk = do
  let keys     = nub $ Map.keys table ++ Map.keys onDisk :: [Table.RowKey]
      lefts    = map (flip Map.lookup table ) keys       :: [Maybe Table.Row]
      rights   = map (flip Map.lookup onDisk) keys       :: [Maybe (Maybe Table.Row, Int)]
      combined = [combine l r | (l, r) <- zip lefts rights]
  Map.fromList $ zip keys $ catMaybes combined
  where combine :: Maybe Table.Row -> Maybe (Maybe Table.Row, Int) -> Maybe (Maybe Table.Row, Int)
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
onChangeRow' :: Table.TableKey -> Table.RowKey -> (Maybe Table.Row -> TFile ()) -> TFile ()
onChangeRow' tableKey rowKey f = liftIO $ do
  path <- canonicalizePath $ tablePath tableKey
  let moddedFile (Modified moddedPath _ _) = path == moddedPath
      moddedFile _                         = False
  manager     <- FS.startManager
  tableOnDisk <- runT $ readTableOnDisk tableKey
  let idEntry = Map.lookup rowKey tableOnDisk :: Maybe (Maybe Table.Row, Int)
  lastIdMVar <- MVar.newMVar $ maybe (-1) snd idEntry
  void $ FS.watchDir manager (takeDirectory path) moddedFile $ const $
    runT $ do
      newTableOnDisk <- readTableOnDisk tableKey
      case Map.lookup rowKey newTableOnDisk of
        -- Still no entry on disk.
        Nothing                -> pure ()
        Just (maybeRow, newId) -> do
          -- Check ID of last known update..
          lastId <- liftIO $ MVar.swapMVar lastIdMVar newId
          -- ..if an update has occured since, run the function.
          when (newId > lastId) (f maybeRow)

-- | Write updates to disk.

-- | Decode a value of type 'a' from a file, or return a default value.
--
-- TODO: replace fromRight' with fromRight and raise DecodeError.
--
-- In-case of a file-not-found error the default value is returned.
readOrDefault :: Serialize a => a -> FilePath -> IO a
readOrDefault default' path =
  flip catch catchDoesNotExistError $ do
    fromRight' . decode . pack <$> Strict.readFile path
  where catchDoesNotExistError e
          | isDoesNotExistError e = pure default'
          | otherwise             = throwIO e
