{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ChatRoom.Common where

import           Data.Text    ( Text )
import           GHC.Generics ( Generic )
import           Telescope    ( PrimaryKey(..) )

data Message = Message
  { mId      :: Int
  , room     :: Text
  , username :: Text
  , message  :: Text
  } deriving (Eq, Ord, Generic, Show)

instance PrimaryKey Message Int where
  primaryKey = mId
