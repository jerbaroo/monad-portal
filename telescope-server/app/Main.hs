{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import           Control.Monad.IO.Class ( liftIO )
import           GHC.Generics           ( Generic )
import qualified Telescope.Ops          as T
import qualified Telescope.Server       as Server
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
  liftIO $ runTFile $ mapM T.set [ Person "John" 3 ]
  print =<< (liftIO $ runTFile $ T.viewTable Person{})
  Server.run 3001
