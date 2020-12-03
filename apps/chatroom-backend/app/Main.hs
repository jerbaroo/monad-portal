{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           ChatRoom.Common       ( Message(..) )
import           Telescope.Class       ( primaryKey )
import qualified Telescope.Operations as T
import qualified Telescope.Server     as Server
import           Telescope.DS.File     ( runT )

main :: IO ()
main = do
  let message = Message 1 "main" "john" "Hello everyone"
  _ <- runT $ T.rmTable @Message >> T.set message
  print =<< runT (T.viewK @Message $ primaryKey message)
  Server.run 3002 Server.developmentCors
