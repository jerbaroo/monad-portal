<div align="center">

# Telescope
[![Build Status](https://img.shields.io/github/workflow/status/jerbaroo/telescope/Test)](https://github.com/jerbaroo/telescope/actions?query=workflow%3ATest)

</div>

# Introduction
*Minimum viable product. Not production ready.*

Telescope is a framework for rapid development of reactive web apps with the
Haskell programming language. Telescope abstracts away the common tasks you
undertake when developing a web app, **allowing you to focus on your business
logic** and **reducing the time you need to build your app**.

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
- Does not provide a full-featured database query language.
- Only supports a limited subset of Haskell data types.

Telescope is particularly well-suited for applications where events are pushed
by the server e.g. notifications and dashboards. Telescope also handles forms
and input-validation very well. On the flip-side, applications with heavy
client-side computation such as animations are not well-suited for Telescope.

## Getting Started
Building a reactive web app with Telescope looks something like this:

**1.** Declare the data types used in your application.

``` haskell
data ToDoList = ToDoList
  { name  :: Text
  , items :: [Text]
  } deriving (Generic, Show)

instance PrimaryKey ToDoList Text where
  primaryKey = name
```

**2.** Populate your database with some data.

``` haskell
T.set $ ToDoList "pancakes" ["eggs", "milk", "flour"]
```

**3.** Start the Telescope server.

``` haskell
Server.run port
```

**4.** Write the frontend of your reactive web app with
[Reflex-DOM](https://reflex-frp.org/)!

``` haskell
-- NOTE: work in progress.
main = mainWidget $ el "div" $ do
  el "h3" $ text "View a todo list"
  inputDyn <- textInput def
  list <- flip T.viewKRx
    (unpack <$> inputDyn ^. textInput_value)
    (const TodoList{} <$> (inputDyn ^. textInput_value))
  dynText $ fmap (pack . show) people
```

**5.** Open the app in two browser tabs. Edit one to-do-list and watch the other
one react!

A full tutorial and demo application are available TODO.
<!-- TODO: links to reflex-platform and other doc in demo/README.md -->
