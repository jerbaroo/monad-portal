{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main ( main ) where

import           Control.Exception       ( try )
import           Control.Monad           ( void, when )
import           Data.Either             ( isRight )
import           GHC.Generics            ( Generic )
import           Telescope.Exception     ( TelescopeException )
import qualified Telescope.Ops          as T
import           Telescope.Table         ( PrimaryKey(..) )
import           Telescope.TFile         ( runTFile )
import qualified Test.HUnit             as HUnit
import qualified System.Exit            as Exit

main :: IO ()
main = do
  results <- HUnit.runTestTT $ HUnit.TestList [
      testSetView
    , testOver
    , testSetTableViewTable
    , testRmRmTable
    , testSetTableDuplicateUsers
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
  runTFile $ T.rmTable Person{} -- Test setup.
  -- View John before set.
  johnMay <- runTFile $ T.view john1
  assertEqualUsers Nothing johnMay
  -- View John after setting john1.
  runTFile $ T.set john1
  johnMay <- runTFile $ T.view john1
  assertEqualUsers (Just john1) johnMay
  -- View John after setting john2.
  runTFile $ T.set john2
  johnMay <- runTFile $ T.view john2
  assertEqualUsers (Just john2) johnMay
  
testOver :: HUnit.Test
testOver = HUnit.TestCase $ do
  runTFile $ T.rmTable Person{} -- Test setup.
  -- View John after modifying non-existing user.
  runTFile $ T.over john1 (\p -> p { age = 21 })
  johnMay <- runTFile $ T.view john1
  assertEqualUsers Nothing johnMay
  -- View John after modifying age.
  runTFile $ T.set john1
  runTFile $ T.over john1 (\p -> p { age = 21 })
  johnMay <- runTFile $ T.view john1
  assertEqualUsers (Just Person { name = "John", age = 21 }) johnMay
  -- View new user after modifying name.
  runTFile $ T.over john1 (\p -> p { name = "Steve" })
  johnMay <- runTFile $ T.view john1
  assertEqualUsers Nothing johnMay
  steveMay <- runTFile $ T.viewK Person{} "Steve"
  assertEqualUsers (Just Person { name = "Steve", age = 21 }) steveMay

testSetTableViewTable :: HUnit.Test
testSetTableViewTable = HUnit.TestCase $ do
  runTFile $ T.rmTable Person{} -- Test setup.
  -- View table before set.
  table <- runTFile $ T.viewTable john1
  assertEqualTables [] table
  -- View table after setting users.
  runTFile $ T.setTable [john1, mary]
  table <- runTFile $ T.viewTable john1
  assertEqualTables [john1, mary] table
  -- View table after overwriting users.
  runTFile $ T.setTable [john2, mary]
  table <- runTFile $ T.viewTable john1
  assertEqualTables [john2, mary] table

testRmRmTable :: HUnit.Test
testRmRmTable = HUnit.TestCase $ do
  runTFile $ T.rmTable Person{} -- Test setup.
  -- View John after remove.
  runTFile $ T.set john1
  runTFile $ T.rm john1
  johnMay <- runTFile $ T.viewK Person{} "John"
  assertEqualUsers Nothing johnMay
  -- View table after removing table.
  runTFile $ T.setTable [john1, mary]
  runTFile $ T.rmTable Person{}
  table <- runTFile $ T.viewTable Person{}
  assertEqualTables [] table
  -- View table after removing 1 user.
  runTFile $ T.setTable [john1, mary]
  runTFile $ T.rm john1
  table <- runTFile $ T.viewTable Person{}
  assertEqualTables [mary] table

testSetTableDuplicateUsers :: HUnit.Test
testSetTableDuplicateUsers = HUnit.TestCase $ do
  runTFile $ T.rmTable Person{} -- Test setup.
  -- No error after setting unique users.
  -- runTFile $ T.setTable [john1, mary]
  -- Error after setting duplicate users.
  -- errored <- try $ runTFile $ T.setTable [john1, john1]
    -- :: IO (Either TelescopeException ())
  -- when (isRight errored) $ HUnit.assertFailure
    -- "No error thrown setting duplicate users with setTable"
