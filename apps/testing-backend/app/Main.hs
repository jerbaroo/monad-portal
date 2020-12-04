{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import qualified Telescope         as T
import qualified Telescope.Server  as Server
import           Telescope.DS.File ( runT )
import           Testing.Common    ( Person(..) )

main :: IO ()
main = do
  _ <- runT $ T.setTable [Person "John" 21]
  print =<< runT (T.viewTable @Person)
  Server.run 3002 Server.developmentCors
