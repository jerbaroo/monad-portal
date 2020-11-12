{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE TypeApplications #-}

module Main where

import           Demo.Common            ( Person(..) )
import qualified Telescope.Ops          as T
import qualified Telescope.Server       as Server
import           Telescope.DS.File      ( runT )

main :: IO ()
main = do
  _ <- runT $ T.setTable [Person "John" 21]
  print =<< runT (T.viewTable @Person)
  Server.run 3002 Server.developmentCors
