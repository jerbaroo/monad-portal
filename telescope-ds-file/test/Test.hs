module Main ( main ) where

import qualified Telescope.DS.File as TFile
import           Telescope.DS.Test  ( runTestSuite )

main :: IO ()
main = runTestSuite TFile.runT
