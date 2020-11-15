{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

-- | A Test-suite for instance of the Telescope interface.
module Telescope.DS.Test where

import           Control.Comonad          ( Comonad )
import           Control.Concurrent       ( threadDelay )
import           Control.Concurrent.MVar as MVar
import           Control.Monad.IO.Class   ( MonadIO, liftIO )
import           Data.Text                ( Text )
import           GHC.Generics             ( Generic )
import           Telescope.Class          ( Telescope )
import qualified Telescope.Operations    as T
import           Telescope.Table.To      as Table
import qualified Test.HUnit              as HUnit
import qualified System.Exit             as Exit

runTestSuite :: (Telescope m f, Comonad f, MonadIO m)
  => (forall a. m a -> IO a) -> IO ()
runTestSuite run = do
  results <- HUnit.runTestTT $ testList run
  if   HUnit.errors results + HUnit.failures results == 0
  then Exit.exitWith   Exit.ExitSuccess
  else Exit.exitWith $ Exit.ExitFailure 1

testList :: (Telescope m f, Comonad f, MonadIO m)
  => (forall a. m a -> IO a) -> HUnit.Test
testList run = HUnit.TestList
    [ testSetView           run
    , testSetTableViewTable run
    -- , testOver           run
    , testRmRmTable         run
    , testOnChange          run
    ]

-- | Data type used for testing.
data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Eq, Generic, Show)

instance Table.PrimaryKey Person Text where
  primaryKey = name

john1 :: Person
john1 = Person "John" 69

john2 :: Person
john2 = Person "John" 70

mary :: Person
mary = Person "Mary" 70

equalTablesMsg :: String
equalTablesMsg = "Viewed table not equal to set table"

equalUsersMsg :: String
equalUsersMsg = "Viewed 'User' not equal to set 'User'"

testSetView :: (Telescope m f, Comonad f) =>
  (forall a. m a -> IO a) -> HUnit.Test
testSetView run = HUnit.TestCase $ do
  run $ T.rmTable @Person -- Test setup.

  -- View John before set.
  johnMay <- run $ T.view john1
  HUnit.assertEqual equalUsersMsg Nothing johnMay

  -- View John after setting john1.
  run $ T.set john1
  johnMay <- run $ T.view john1
  HUnit.assertEqual equalUsersMsg (Just john1) johnMay

  -- View John after setting john2.
  run $ T.set john2
  johnMay <- run $ T.view john2
  HUnit.assertEqual equalUsersMsg (Just john2) johnMay

testSetTableViewTable :: (Telescope m f, Comonad f) =>
  (forall a. m a -> IO a) -> HUnit.Test
testSetTableViewTable run = HUnit.TestCase $ do
  run $ T.rmTable @Person -- Test setup.

  -- View table before set.
  table <- run $ T.viewTable @Person
  HUnit.assertEqual equalTablesMsg [] table

  -- View table after setting users.
  run $ T.setTable [john1, mary]
  table <- run $ T.viewTable
  HUnit.assertEqual equalTablesMsg [john1, mary] table

  -- View table after overwriting users.
  run $ T.setTable [john2, mary]
  table <- run $ T.viewTable
  HUnit.assertEqual equalTablesMsg [john2, mary] table

-- testOver :: (forall a m. MonadIO m => m a -> m a) -> HUnit.Test
-- testOver run = HUnit.TestCase $ do
--   run $ T.rmTable @Person -- Test setup.

--   -- View John after modifying non-existing user.
--   run $ T.over john1 (\p -> p { age = 21 })
--   johnMay <- run $ T.view john1
--   HUnit.assertEqual equalUsersMsg Nothing johnMay

--   -- View John after modifying age.
--   run $ T.set john1
--   run $ T.over john1 (\p -> p { age = 21 })
--   johnMay <- run $ T.view john1
--   HUnit.assertEqual equalUsersMsg (Just Person { name = "John", age = 21 }) johnMay

--   -- View new user after modifying name.
--   run $ T.over john1 (\p -> p { name = "Steve" })
--   johnMay <- run $ T.view john1
--   HUnit.assertEqual equalUsersMsg Nothing johnMay
--   steveMay <- run $ T.viewK Person{} "Steve"
--   HUnit.assertEqual equalUsersMsg (Just Person { name = "Steve", age = 21 }) steveMay

testRmRmTable :: (Telescope m f, Comonad f) =>
  (forall a. m a -> IO a) -> HUnit.Test
testRmRmTable run = HUnit.TestCase $ do
  run $ T.rmTable @Person -- Test setup.

  -- View John after remove.
  run $ T.set john1
  run $ T.rm john1
  (johnMay :: Maybe Person) <- run $ T.viewK "John"
  HUnit.assertEqual equalUsersMsg Nothing johnMay

  -- View table after removing table.
  run $ T.setTable [john1, mary]
  run $ T.rmTable @Person
  table <- run $ T.viewTable @Person
  HUnit.assertEqual equalTablesMsg [] table

  -- View table after removing 1 user.
  run $ T.setTable [john1, mary]
  run $ T.rm john1
  table <- run $ T.viewTable
  HUnit.assertEqual equalTablesMsg [mary] table

testOnChange :: (Telescope m f, Comonad f, MonadIO m) =>
  (forall a. m a -> IO a) -> HUnit.Test
testOnChange run = HUnit.TestCase $ do
  run $ T.rmTable @Person -- Test setup.

  -- MVar to record changes.
  changesMVar <- MVar.newMVar (Nothing, 0 :: Int)
  let delayAndRead = threadDelay 1000000 >> MVar.readMVar changesMVar

  -- Register on change function.
  run $ T.onChange "John" $ \p -> liftIO $ do
    (_, lastCount) <- MVar.takeMVar changesMVar
    MVar.putMVar changesMVar (p, lastCount + 1)

  -- Assert 'onChange' runs after initial 'set'.
  run $ T.set $ john1
  (lastPerson, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' did NOT record 'set' (initial)"
    (Just john1) lastPerson
  HUnit.assertEqual "'onChange' did NOT record 'set' (initial)" 1 lastCount

  -- Assert 'onChange' runs after setting new value with same key.
  run $ T.set john2
  (lastPerson, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' did NOT record 'set' (overwrite)"
    (Just john2) lastPerson
  HUnit.assertEqual "'onChange' did NOT record 'set' (overwrite)" 2 lastCount

  -- Assert 'onChange' does NOT run after setting same value again.
  run $ T.set john2
  (_, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' DID record 'set' (duplicate)" 2 lastCount

  -- Assert 'onChange' runs after removing value.
  run $ T.rm john2
  (lastPerson, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' did NOT record 'rm'" Nothing lastPerson
  HUnit.assertEqual "'onChange' did NOT record 'rm'" 3 lastCount

  -- Assert 'onChange' does NOT run after removing value again.
  run $ T.rm john2
  (_, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' DID record 'rm' (duplicate)" 3 lastCount
