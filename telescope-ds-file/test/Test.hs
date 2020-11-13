{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main ( main ) where

import           Control.Concurrent       ( threadDelay )
import           Control.Concurrent.MVar as MVar
import           Control.Monad.IO.Class   ( liftIO )
import           Data.Text                ( Text )
import           GHC.Generics             ( Generic )
import qualified Telescope.Ops           as T
import           Telescope.Table          ( PrimaryKey(..) )
import           Telescope.DS.File        ( runT )
import qualified Test.HUnit              as HUnit
import qualified System.Exit             as Exit

main :: IO ()
main = do
  results <- HUnit.runTestTT $ HUnit.TestList
    [ testSetView
    , testSetTableViewTable
    , testOver
    , testRmRmTable
    , testOnChange
    ]
  if   HUnit.errors results + HUnit.failures results == 0
  then Exit.exitWith   Exit.ExitSuccess
  else Exit.exitWith $ Exit.ExitFailure 1

-- | Datatype used for testing.
data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Eq, Generic, Show)

instance PrimaryKey Person Text where
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

testSetView :: HUnit.Test
testSetView = HUnit.TestCase $ do
  runT $ T.rmTable @Person -- Test setup.

  -- View John before set.
  johnMay <- runT $ T.view john1
  HUnit.assertEqual equalUsersMsg Nothing johnMay

  -- View John after setting john1.
  runT $ T.set john1
  johnMay <- runT $ T.view john1
  HUnit.assertEqual equalUsersMsg (Just john1) johnMay

  -- View John after setting john2.
  runT $ T.set john2
  johnMay <- runT $ T.view john2
  HUnit.assertEqual equalUsersMsg (Just john2) johnMay

testSetTableViewTable :: HUnit.Test
testSetTableViewTable = HUnit.TestCase $ do
  runT $ T.rmTable @Person -- Test setup.

  -- View table before set.
  table <- runT $ T.viewTable @Person
  HUnit.assertEqual equalTablesMsg [] table

  -- View table after setting users.
  runT $ T.setTable [john1, mary]
  table <- runT $ T.viewTable
  HUnit.assertEqual equalTablesMsg [john1, mary] table

  -- View table after overwriting users.
  runT $ T.setTable [john2, mary]
  table <- runT $ T.viewTable
  HUnit.assertEqual equalTablesMsg [john2, mary] table

testOver :: HUnit.Test
testOver = HUnit.TestCase $ do
  runT $ T.rmTable @Person -- Test setup.

  -- View John after modifying non-existing user.
  -- runT $ T.over john1 (\p -> p { age = 21 })
  -- johnMay <- runT $ T.view john1
  -- HUnit.assertEqual equalUsersMsg Nothing johnMay

  -- -- View John after modifying age.
  -- runT $ T.set john1
  -- runT $ T.over john1 (\p -> p { age = 21 })
  -- johnMay <- runT $ T.view john1
  -- HUnit.assertEqual equalUsersMsg (Just Person { name = "John", age = 21 }) johnMay

  -- -- View new user after modifying name.
  -- runT $ T.over john1 (\p -> p { name = "Steve" })
  -- johnMay <- runT $ T.view john1
  -- HUnit.assertEqual equalUsersMsg Nothing johnMay
  -- steveMay <- runT $ T.viewK Person{} "Steve"
  -- HUnit.assertEqual equalUsersMsg (Just Person { name = "Steve", age = 21 }) steveMay

testRmRmTable :: HUnit.Test
testRmRmTable = HUnit.TestCase $ do
  runT $ T.rmTable @Person -- Test setup.

  -- View John after remove.
  runT $ T.set john1
  runT $ T.rm john1
  (johnMay :: Maybe Person) <- runT $ T.viewK "John"
  HUnit.assertEqual equalUsersMsg Nothing johnMay

  -- View table after removing table.
  runT $ T.setTable [john1, mary]
  runT $ T.rmTable @Person
  table <- runT $ T.viewTable @Person
  HUnit.assertEqual equalTablesMsg [] table

  -- View table after removing 1 user.
  runT $ T.setTable [john1, mary]
  runT $ T.rm john1
  table <- runT $ T.viewTable
  HUnit.assertEqual equalTablesMsg [mary] table

testOnChange :: HUnit.Test
testOnChange = HUnit.TestCase $ do
  runT $ T.rmTable @Person -- Test setup.

  -- MVar to record changes.
  changesMVar <- MVar.newMVar (Nothing, 0 :: Int)
  let delayAndRead = threadDelay 1000000 >> MVar.readMVar changesMVar

  -- Register on change function.
  runT $ T.onChange "John" $ \p -> liftIO $ do
    (_, lastCount) <- MVar.takeMVar changesMVar
    MVar.putMVar changesMVar (p, lastCount + 1)

  -- Assert 'onChange' runs after initial 'set'.
  runT $ T.set $ john1
  (lastPerson, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' did NOT record 'set' (initial)"
    (Just john1) lastPerson
  HUnit.assertEqual "'onChange' did NOT record 'set' (initial)" 1 lastCount

  -- Assert 'onChange' runs after setting new value with same key.
  runT $ T.set john2
  (lastPerson, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' did NOT record 'set' (overwrite)"
    (Just john2) lastPerson
  HUnit.assertEqual "'onChange' did NOT record 'set' (overwrite)" 2 lastCount

  -- Assert 'onChange' does NOT run after setting same value again.
  runT $ T.set john2
  (_, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' DID record 'set' (duplicate)" 2 lastCount

  -- Assert 'onChange' runs after removing value.
  runT $ T.rm john2
  (lastPerson, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' did NOT record 'rm'" Nothing lastPerson
  HUnit.assertEqual "'onChange' did NOT record 'rm'" 3 lastCount

  -- Assert 'onChange' does NOT run after removing value again.
  runT $ T.rm john2
  (_, lastCount) <- delayAndRead
  HUnit.assertEqual "'onChange' DID record 'rm' (duplicate)" 3 lastCount
