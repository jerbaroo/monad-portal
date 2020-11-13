{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Demo.Common where

import           Data.Text              ( Text )
import           GHC.Generics           ( Generic )
import           Telescope.Table        ( PrimaryKey(..) )

data Person = Person {
    name :: Text
  , age  :: Int
  } deriving (Generic, Show)

instance PrimaryKey Person Text where
  primaryKey (Person name age) = name
