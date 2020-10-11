{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Concurrent     ( threadDelay )
import           Control.Monad.IO.Class ( liftIO )
import           GHC.Generics           ( Generic )
import qualified Telescope.Ops          as T
import           Telescope.Table        ( PrimaryKey(..) )
import           Telescope.DS.File      ( runTFile )

data Person = Person {
    name :: String
  , age  :: Int
  } deriving (Generic, Show)

instance PrimaryKey Person String where
  primaryKey (Person name age) = name

main :: IO ()
main = do
  let john = Person "John" 69
  runTFile $ T.onChange john $ liftIO . print
  runTFile $ T.set $ Person "John" 21
  threadDelay 100000
  runTFile $ T.set $ Person "John" 22
  threadDelay 100000
  runTFile $ T.set $ Person "John" 22
  threadDelay 100000
  runTFile $ T.rm john
  threadDelay 100000
