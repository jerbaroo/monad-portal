module Main where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Portal (PrimaryKey(..))
import Data.Portal.Storable.To
import Data.Portal.Storable.Types
import Data.Portal.Table.To
import Data.Portal.Table.Types
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = pPrint maryT

data User = User
  { name :: Text
  , age :: Int
  , child :: Maybe User
  } deriving Generic

instance PrimaryKey User Text where
  primaryKey = name

john :: User
john = User "John" 21 Nothing

johnS :: SDataType
johnS = toSDataType john

johnT :: Rows
johnT = sToRows johnS

mary :: User
mary = User "Mary" 51 $ Just john

maryS :: SDataType
maryS = toSDataType mary

maryT :: Rows
maryT = sToRows maryS
