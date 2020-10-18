{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Main where

import           Demo.Common            ( Person(..) )
import qualified Telescope.Ops          as T
import qualified Telescope.Server       as Server
import           Telescope.DS.File      ( runT )

main :: IO ()
main = do
  runT $ mapM T.set [ Person "John" 69 ]
  print =<< runT (T.viewTable Person{})
  Server.run 3002

{-

To test websocket in a browser:

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
