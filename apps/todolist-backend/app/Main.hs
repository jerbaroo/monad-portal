{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           ToDoList.Common    ( ToDoList(..) )
import qualified Telescope         as T
import qualified Telescope.Server  as Server
import           Telescope.DS.File  ( runT )

main :: IO ()
main = do
  _ <- runT $ do
    T.rmTable @ToDoList
    T.set $ ToDoList "p" ["eggs", "milk", "flour"]
  print =<< runT (T.viewK @ToDoList "pancakes")
  Server.run 3002 Server.developmentCors
