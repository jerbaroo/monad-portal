{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           GHC.Generics                   ( Generic )
import qualified Telescope.Ops                  as T
import           Telescope.Table                ( PKey(..) )
import           Telescope.TFile                ( runTFile )

data Person = Person {
    name :: String
  , age  :: Int
  } deriving (Generic, Show)

instance PKey Person String where
  pKey (Person name age) = name

main :: IO ()
main = do
  let john = Person "John" 69
  x <- runTFile $ do
    T.set john
    -- T.viewR Person{} "John"
    T.view john
  print x
