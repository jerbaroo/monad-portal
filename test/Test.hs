{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main ( main ) where

import           Control.Monad          ( void, when )
import           Control.Monad.IO.Class ( liftIO )
import           GHC.Generics           ( Generic )
import qualified Telescope.Ops         as T
import           Telescope.Table        ( PKey(..) )
import           Telescope.TFile        ( runTFile )
import qualified Test.HUnit            as HUnit

main :: IO ()
main = void $ HUnit.runTestTT $ HUnit.TestList [
    testSetView
  ]

data Person = Person {
    name :: String
  , age  :: Int
  } deriving (Eq, Generic, Show)

instance PKey Person String where
  pKey (Person name age) = name

testSetView = HUnit.TestCase $ do
  let john = Person "John" 69
  runTFile $ T.set john
  johnMay <- runTFile $ T.view john
  liftIO $ HUnit.assertEqual "Viewed 'User' not equal to set 'User'"
    (Just john) johnMay
