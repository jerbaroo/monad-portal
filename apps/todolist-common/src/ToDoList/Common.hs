{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ToDoList.Common where

import           Data.Text       ( Text )
import           GHC.Generics    ( Generic )
import           Telescope.Class ( PrimaryKey(..) )

data ToDoList = ToDoList
  { name  :: Text
  , items :: [Text]
  } deriving (Generic, Show)

instance PrimaryKey ToDoList Text where
  primaryKey = name
