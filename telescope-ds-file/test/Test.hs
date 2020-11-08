{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main ( main ) where

import           Control.Concurrent       ( threadDelay )
import           Control.Concurrent.MVar as MVar
import           Control.Exception        ( try )
import           Control.Monad            ( void, when )
import           Control.Monad.IO.Class   ( liftIO )
import           Data.Either              ( isRight )
import           GHC.Generics             ( Generic )
import           Telescope.Exception      ( TelescopeException )
import qualified Telescope.Ops           as T
import           Telescope.Table          ( PrimaryKey(..) )
import           Telescope.DS.File        ( runT )
import qualified Test.HUnit              as HUnit
import qualified System.Exit             as Exit

main :: IO ()
main = do
  results <- HUnit.runTestTT $ HUnit.TestList [
      testSetView
    , testOver
    , testSetTableViewTable
    , testRmRmTable
    , testSetTableDuplicateUsers
    , testOnChange
    ]
  if   HUnit.errors results + HUnit.failures results == 0
  then Exit.exitWith Exit.ExitSuccess
  else Exit.exitWith $ Exit.ExitFailure 1

data Person = Person {
    name :: String
  , age  :: Int
  } deriving (Eq, Generic, Show)

instance PrimaryKey Person String where
  primaryKey (Person name age) = name
  
john1 = Person "John" 69
john2 = Person "John" 70
mary  = Person "Mary" 70
assertEqualTables a b =
  HUnit.assertEqual "Viewed table not equal to set table" a b
assertEqualUsers a b =
  HUnit.assertEqual "Viewed 'User' not equal to set 'User'" a b

testSetView :: HUnit.Test
testSetView = HUnit.TestCase $ do
  runT $ T.rmTable Person{} -- Test setup.
  -- View John before set.
  johnMay <- runT $ T.view john1
  assertEqualUsers Nothing johnMay
  -- View John after setting john1.
  runT $ T.set john1
  johnMay <- runT $ T.view john1
  assertEqualUsers (Just john1) johnMay
  -- View John after setting john2.
  runT $ T.set john2
  johnMay <- runT $ T.view john2
  assertEqualUsers (Just john2) johnMay
  
testOver :: HUnit.Test
testOver = HUnit.TestCase $ do
  pure ()
--   runT $ T.rmTable Person{} -- Test setup.
--   -- View John after modifying non-existing user.
--   runT $ T.over john1 (\p -> p { age = 21 })
--   johnMay <- runT $ T.view john1
--   assertEqualUsers Nothing johnMay
--   -- View John after modifying age.
--   runT $ T.set john1
--   runT $ T.over john1 (\p -> p { age = 21 })
--   johnMay <- runT $ T.view john1
--   assertEqualUsers (Just Person { name = "John", age = 21 }) johnMay
--   -- View new user after modifying name.
--   runT $ T.over john1 (\p -> p { name = "Steve" })
--   johnMay <- runT $ T.view john1
--   assertEqualUsers Nothing johnMay
--   steveMay <- runT $ T.viewK Person{} "Steve"
--   assertEqualUsers (Just Person { name = "Steve", age = 21 }) steveMay

testSetTableViewTable :: HUnit.Test
testSetTableViewTable = HUnit.TestCase $ do
  runT $ T.rmTable Person{} -- Test setup.
  -- View table before set.
  table <- runT $ T.viewTable john1
  assertEqualTables [] table
  -- View table after setting users.
  runT $ T.setTable [john1, mary]
  table <- runT $ T.viewTable john1
  assertEqualTables [john1, mary] table
  -- View table after overwriting users.
  runT $ T.setTable [john2, mary]
  table <- runT $ T.viewTable john1
  assertEqualTables [john2, mary] table

testRmRmTable :: HUnit.Test
testRmRmTable = HUnit.TestCase $ do
  runT $ T.rmTable Person{} -- Test setup.
  -- View John after remove.
  runT $ T.set john1
  runT $ T.rm john1
  johnMay <- runT $ T.viewK Person{} "John"
  assertEqualUsers Nothing johnMay
  -- View table after removing table.
  runT $ T.setTable [john1, mary]
  runT $ T.rmTable Person{}
  table <- runT $ T.viewTable Person{}
  assertEqualTables [] table
  -- View table after removing 1 user.
  runT $ T.setTable [john1, mary]
  runT $ T.rm john1
  table <- runT $ T.viewTable Person{}
  assertEqualTables [mary] table

testSetTableDuplicateUsers :: HUnit.Test
testSetTableDuplicateUsers = HUnit.TestCase $ do
  runT $ T.rmTable Person{} -- Test setup.
  -- No error after setting unique users.
  -- runT $ T.setTable [john1, mary]
  -- Error after setting duplicate users.
  -- errored <- try $ runT $ T.setTable [john1, john1]
    -- :: IO (Either TelescopeException ())
  -- when (isRight errored) $ HUnit.assertFailure
    -- "No error thrown setting duplicate users with setTable"

testOnChange :: HUnit.Test
testOnChange = HUnit.TestCase $ do
  runT $ T.rmTable Person{} -- Test setup.
  -- MVar to record changes.
  changesMVar <- MVar.newMVar (Nothing, 0)
  let delayAndRead = threadDelay 100000 >> MVar.readMVar changesMVar
  -- Register on change function.
  runT $ T.onChange john1 $ \p -> liftIO $ do
    (_, lastCount) <- MVar.takeMVar changesMVar
    MVar.putMVar changesMVar (p, lastCount + 1)
  -- Assert 'onChange' runs after initial 'set'.
  runT $ T.set $ john1
  (lastPerson, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' did not record initial 'set'"
    (Just john1) lastPerson
  HUnit.assertEqual "'onChange' did not record initial 'set'" 1 lastCount
  -- Assert 'onChange' runs after setting new value with same key.  
  runT $ T.set john2
  (lastPerson, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' did not record further 'set'"
    (Just john2) lastPerson
  HUnit.assertEqual "'onChange' did not record further 'set'" 2 lastCount
  -- Assert 'onChange' does NOT run after setting same value again.  
  runT $ T.set john2
  (lastPerson, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' did record duplicate 'set'" 2 lastCount
  -- Assert 'onChange' runs after removing value.
  runT $ T.rm john2
  (lastPerson, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' did not record 'rm'" Nothing lastPerson
  HUnit.assertEqual "'onChange' did not record 'rm'" 3 lastCount
  -- Assert 'onChange' does NOT run after removing value again.
  runT $ T.rm john2
  (lastPerson, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' did record 'rm'" 3 lastCount
  
