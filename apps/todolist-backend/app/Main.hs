{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           ToDoList.Common       ( ToDoList(..) )
import qualified Telescope.Operations as T
import qualified Telescope.Server     as Server
import           Telescope.DS.File     ( runT )

main :: IO ()
main = do
  _ <- runT $ T.set $ ToDoList "pancakes" ["eggs", "milk", "flour"]
  print =<< runT (T.viewK @ToDoList "pancakes")
  Server.run 3002 Server.developmentCors
