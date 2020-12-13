{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ChatRoom.Common where

import           Data.Text    ( Text )
import           GHC.Generics ( Generic )
import           Telescope    ( PrimaryKey(..) )

data Message = Message
  { time     :: Int
  , room     :: Text
  , username :: Text
  , message  :: Text
  } deriving (Eq, Ord, Generic, Show)

instance PrimaryKey Message Int where primaryKey = time
