{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Telescope.TFile where

import           Control.Exception       ( catch, throwIO )
import           Control.Monad.IO.Class  ( MonadIO, liftIO )
import           Data.ByteString.Char8   ( pack, unpack )
import           Data.Either.Extra       ( fromRight' )
import qualified Data.Map               as Map
import           Data.Serialize          ( Serialize, decode, encode )
import           System.IO.Error         ( isDoesNotExistError )
import qualified System.IO.Strict       as Strict
import           Telescope.Class         ( Telescope(..) )
import qualified Telescope.Table        as Table

-- ^ A naive file-backed data source, functional but slow.
--
-- TODO: Rename to DSFile.
-- TODO: Use MonadReader to hold path to data source.
--
-- For each data type, 'TFile' maintains one file where all instances of that
-- data type are stored. Inside one of these files the data type instances are
-- stored as one serialized nested map: {RowKey: {ColumnKey: value}}.
newtype TFile a = TFile (IO a) deriving (Functor, Applicative, Monad, MonadIO)

instance Telescope TFile where
  viewTable (Table.TableKey tkName) = do
    liftIO $ readOrDefault Map.empty (tkName ++ ".table")
  setTable (Table.TableKey tkName) table = do
    liftIO $ writeFile (tkName ++ ".table") $ unpack $ encode table

-- | Decode a value of type 'a' from a file, or return a default value.
--
-- TODO: replace fromRight' with fromRight and raise DecodeError.
--
-- In-case of a file-not-found error the default value is returned.
readOrDefault :: Serialize a => a -> FilePath -> IO a
readOrDefault default' updatesPath =
  flip catch catchDoesNotExistError $ do
    fromRight' . decode . pack <$> Strict.readFile updatesPath
  where catchDoesNotExistError e
          | isDoesNotExistError e = pure default'
          | otherwise             = throwIO e

runTFile :: MonadIO m => TFile a -> m a
runTFile (TFile a) = liftIO a
