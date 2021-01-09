# Tutorial
In this tutorial we will walk through some of the features that Telescope
provides. The first few features will be presented as if we were building a
simple chat room application. Note that this is not a tutorial on Reflex. If you
are new to Reflex you may find some of the following links helpful:

- [Reflex-DOM Calculator Tutorial](https://github.com/reflex-frp/reflex-platform/blob/develop/README.md#tutorial)
- [Reflex Project Development](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst)
- [Reflex Quick Reference](https://github.com/reflex-frp/reflex/blob/develop/Quickref.md)
- [Reflex-DOM Quick Reference](https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md)

## Installation
First install [Nix](https://nixos.org/download.html) the package manager, and
install [Cachix](https://docs.cachix.org/) to make use of our binary cache. Then
clone this repo (with submodules) and change in to the `telescope` directory.
You will also want to configure use of the Reflex-FRP cache, follow step 2
[here](https://github.com/obsidiansystems/obelisk#installing-obelisk). Finally
download pre-built binaries with Cachix, and perform an initial build:

``` bash
curl -L https://nixos.org/nix/install | sh
nix-env -iA cachix -f https://cachix.org/api/v1/install
git clone --recurse-submodules https://github.com/jerbaroo/telescope
cd telescope
./scripts/cachix/use.sh # Will take a long time the first time.
```

## Building & Running
The Telescope repo provides example projects which you can build or browse the
source code of. One of these is a /very/ simple chat room web application. To
build and run the chat room application execute the commands below. These two
commands will start a development server each, the first builds and runs your
app's backend server, reloading everytime the source code changes. And the
second builds and runs a server that serves your frontend, reloading everytime
the source code changes. Visit `localhost:3003` to see the app in your browser.

``` bash
./scripts/run/dev.sh chatroom-backend
./scripts/run/dev.sh chatroom-frontend
```

When you are ready to run the application in production you can run the commands
below. These commands will build your frontend files (HTML and JavaScript), and
build and run your backend server. Visit `localhost:3002` to see the app in your
browser.

``` bash
./scripts/build/prod.sh chatroom-frontend
./scripts/build/prod.sh chatroom-backend
./scripts/run/prod.sh   chatroom-backend
```

## Defining Data Types
Telescope applications are primarily built as functions of data that is stored
in a database. Therefore we need to define data types that model our application
while keeping in mind that the data will be stored in a database. A simple
method of storing our data is to store each chat room message as a row in a
database along with any information associated with the message, such as: time
the message was sent, who sent the message, and what chat room the message was
sent in. The data type below captures these requirements. Each field of the data
type will be stored in one column of a table `"Message"` in the database. The
following code which defines the `Message` data type for our chat room app is
available in `telescope/apps/chatroom-common/src/ChatRoom/Common.hs`.

``` haskell
data Message = Message
  { time     :: Int
  , room     :: Text
  , username :: Text
  , message  :: Text
  } deriving (Eq, Ord, Generic, Show)

instance PrimaryKey Message Int where
  primaryKey = time
```

## Database Operations 
During development you might want to insert some data into your database for
testing. Or when running in production you might want to perform some database
maintenance. Telescope provides a number of operations for interacting with your
database, these operations are available both server-side and client-side. These
operations are available in the `Telescope.Operations` module. The following
backend code is available in `telescope/apps/chatroom-backend/app/Main.hs`.

``` haskell
main = do
  let msg = Message 1 "main" "john" "Hello everyone"
  -- You might find the following pattern useful in development. Running this
  -- code before the server starts will remove all messages and insert 'msg'.
  runT $ T.rmTable @Message >> T.set msg
  -- ...
```

## Telescope's Server
Telescope exports a server which understands data types that are an instance of
the `Entity` typeclass, such as the `Message` data type defined above for our
chat room example. When using Telescope operations client-side, the data types
are decomposed into storable representation and sent to the server, which then
completes the operation server-side. Running an instance of Telescope's server
is super simple. The following backend code to start a server is available in
`telescope/apps/chatroom-backend/app/Main.hs`.

``` haskell
main = do
  -- ...
  Server.run 3002 Server.developmentCors
```

## Reflex-DOM Frontend
To complete our chat room application, we just need a frontend. The Telescope
interface is not specific to any frontend, however Telescope currently only
provides integrations with Reflex-DOM. In our chat room frontend we will `view`
messages live from the database, and anytime the user presses enter we will
`set` their message into the database. As previously mentioned this is not a
tutorial on Reflex-DOM, so we won't be discussing the whole frontend. You can
find this frontend code in `telescope/apps/chatroom-frontend/app/Main.hs`.

Notice the use of the functions `T.viewTableRx` and `T.setRx` in the code below.
These Telescope operations will respectively, "view" an entire table live from
the database, and set a stream of data into the database.

``` haskell
main = mainWidget $ do
  -- A text field to enter chat room name and username.
  (roomNameDyn, usernameDyn) <- do
    roomNameInput <- textInputPlaceholder "Chat Room"
    usernameInput <- textInputPlaceholder "Username"
    pure (roomNameInput ^. textInput_value, usernameInput ^. textInput_value)
  -- View messages live from the database.
  dbMessagesEvn <- T.viewTableRx $ const (Proxy @Message) <$> updated roomNameDyn
  -- Filter messages to the current chat room.
  roomMessagesDyn <- holdDyn [] $ attachPromptlyDynWith
    (\rn ms -> [m | m <- ms, room m == rn]) roomNameDyn dbMessagesEvn
  -- Display messages for the current chat room.
  el "ul" $ simpleList roomMessagesDyn $ el "li" . dynText . fmap
    (\m -> "“" <> username m <> "”: " <> message m)
  -- A text field for entering messages, and button to send the message.
  messageTextDyn <- (^. textInput_value) <$> textInputPlaceholder "Enter Message"
  timeEvn        <- fmap (fmap round) . tagTime =<< button "Send"
  -- Construct a 'Message' from user input, and send on button click.
  let messageToSendDyn = (\room username message time -> Message {..})
        <$> roomNameDyn <*> usernameDyn <*> messageTextDyn
      messageToSendEvn = attachPromptlyDynWith ($) messageToSendDyn timeEvn
  T.setRx messageToSendEvn
```

## Server Integration
Telescope is useful when you have data that is uniquely determined by a primary
key, Telescope helps you store that data in a database and manipulate/access
that data. However it may be the case that you don't want to use Telescope to
develop part of your server's API, perhaps you already have a server that you
don't want to rewrite, perhaps you have data that you want to serve but not
store in a database, or perhaps you simply prefer another solution to develop
part of your API.

The server that Telescope exports is part of the Servant package ([Hackage
link](https://hackage.haskell.org/package/servant-server-0.18.2/docs/Servant-Server.html#t:Server)).
If your additional server is a Servant `Server` it can very easily be combined
with Telescope's server, see the code below. On the other hand if your existing
server is not a Servant `Server` then you have two options. One option is to
expose both servers to the client. The second option is to setup your server to
proxy all requests intended for Telescope to Telescope's server. The following
code integrating an existing `Server` with Telescope's `Server` is available in
`telescope/apps/chatroom-backend/app/Main.hs`. The frontend code that interacts
with the integrated server is in `telescope/apps/chatroom-frontend/app/Main.hs`.

``` haskell
main = do
  -- ...
  Warp.run 3002 $ Server.developmentCors $
    Servant.serve (Proxy :: Proxy API) (additionalServer :<|> Server.server)

-- | Additional server with only one endpoint, for demonstrative purposes.
additionalServer :: Servant.Server AdditionalAPI
additionalServer = pure "This is data returned by the additional server"

type AdditionalAPI = "additional" :> Get '[JSON] Text
type API = AdditionalAPI :<|> Telescope.API
```

## Custom Handlers
## Authentication
## Authorization
## Nested Data Types
## Multiple Data Constructors
## Avoiding Infinite Recursion
## Migrations
## reflex-vty
