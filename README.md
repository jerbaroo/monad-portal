<div align="center">

# Telescope
[![Build Status](https://img.shields.io/github/workflow/status/jerbaroo/telescope/Test)](https://github.com/jerbaroo/telescope/actions?query=workflow%3ATest)
[![Netlify Status](https://api.netlify.com/api/v1/badges/b42ff31b-1036-424b-8f24-419de5b62549/deploy-status)](https://app.netlify.com/sites/telescope-hs/deploys)
[![Documentation](https://img.shields.io/badge/-documentation-5e5086)](https://telescope-hs.netlify.app)
[![GitHub Stars](https://img.shields.io/github/stars/jerbaroo/telescope?style=social)](https://github.com/jerbaroo/telescope)

</div>

## Introduction
*Minimum viable product. Not production ready.*

Telescope is a Haskell framework for building reactive applications. Apps built
with Telescope react to changes in your database, so they are always up-to-date.
The Telescope framework abstracts away some of the common tasks when developing
an application, **allowing you to focus on your business logic** and **reducing
the time you need to build your reactive app!**

An application built with Telescope is..
- **Reactive:** don't worry about keeping client-side and server-side data in
  sync, your frontend can automatically react to changes in your database and
  your database can be updated seamlessly by your frontend!
- **Robust:** writing the strongly-typed language Haskell across the stack
  prevents server/client protocol mismatches and other run-time errors, allowing
  you to move fast and not break things.
- **Minimal:** Telescope can setup a database and server for you and also manage
  communication between client and server, so you can focus on the parts of your
  application that really matter.

What are Telescope's limitations?
- Telescope does not provide a full-featured database query language.
- Telescope only supports a limited subset of Haskell data types.

Telescope is particularly well-suited for applications where events are pushed
by the server e.g. notifications and dashboards. Telescope also handles forms
and input-validation very well. On the flip-side, applications with heavy
client-side computation such as animations are not well-suited for Telescope.

In short the way Telescope works: Telescope derives a schema for your data types
via `Generics`. Telescope exports functions F to manipulate data types in your
database. Telescope exports a server which acts as a proxy to the database for
any web clients, allowing web clients to execute the functions F as if they were
server-side. The `Telescope` typeclass determines how to communicate with a
database, Telescope is not specific to any one database (e.g. MongoDB) or
frontend library (e.g. Reflex-DOM). "Reactive" variants of the functions F are
provided, where function parameters and return values are streams of data (e.g.
`Event Int`) as opposed to single values (e.g. `Int`).

## Getting Started
To see what building a reactive application with Telescope looks like, we will
build a simple, yet functional, public chat room. Currently Telescope only has
support for [Reflex-DOM](https://reflex-frp.org/) as a frontend, though support
for [reflex-vty](https://hackage.haskell.org/package/reflex-vty) is planned.

**1.** Define the data types used in your application.

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

**2.** Include Telescope's server in your backend code.

``` haskell
Server.run port id
```

**3.** Write your frontend with [Reflex-DOM](https://reflex-frp.org/)!

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

Finally build and run the application. You can now open the app in two browser
tabs, interact with the app in one tab and watch the other app react!

A full tutorial is available [here](https://telescope-hs.netlify.app/#Tutorial).
In the tutorial we will develop a more full-featured version of this chat room
application that utilizes some additional features of the Telescope framework.

## Contributing
Feedback, questions and contributions are all very welcome! The instructions
[here](https://github.com/jerbaroo/telescope/blob/master/docs/DEVELOPMENT.md)
should contain all the commands you need to get started hacking on this project.
If you want to open an issue you can do that [here](https://github.com/jerbaroo/telescope/issues/new).
