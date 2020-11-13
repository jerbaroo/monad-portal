{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Testing.Common where

import           Data.Text       ( Text )
import           GHC.Generics    ( Generic )
import           Telescope.Class ( PrimaryKey(..) )

data Person = Person {
    name :: Text
  , age  :: Int
  } deriving (Generic, Show)

instance PrimaryKey Person Text where
  primaryKey = name
