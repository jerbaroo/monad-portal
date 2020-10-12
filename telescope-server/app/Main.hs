{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import           Control.Monad.IO.Class ( liftIO )
import           GHC.Generics           ( Generic )
import qualified Telescope.Ops          as T
import qualified Telescope.Server       as Server
import           Telescope.Table        ( PrimaryKey(..) )
import           Telescope.DS.File      ( runTFile )

data Person = Person {
    name :: String
  , age  :: Int
  } deriving (Generic, Show)

instance PrimaryKey Person String where
  primaryKey (Person name age) = name

main :: IO ()
main = do
  liftIO $ runTFile $ mapM T.set [ Person "John" 3 ]
  print =<< (liftIO $ runTFile $ T.viewTable Person{})
  Server.run 3001

{-
To test websocket in browser:

function newws() {
  var ws = new WebSocket("ws://localhost:3001/update");
  ws.onopen = function() { console.log("Opened"); };
  ws.onmessage = function (evt) {
     var received_msg = evt.data;
     console.log("Message received...");
     console.log(received_msg);
  };
  ws.onclose = function() {
     // websocket is closed.
     console.log("Connection closed");
  };
  return ws;
};
var ws = newws();
ws.send("(TableKey \"Person\",RowKey (KeyOne (PString \"John\")))");

-}
