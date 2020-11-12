{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Demo.Common where

import           GHC.Generics           ( Generic )
import           Telescope.Table        ( PrimaryKey(..) )

data Person = Person {
    name :: String
  , age  :: Int
  } deriving (Generic, Show)

instance PrimaryKey Person String where
  primaryKey (Person name age) = name
