<div align="center">

# Telescope

</div>

### Table of Contents
- [Introduction](#introduction)
- [Getting Started](#getting-started)
- [Application Architecture](#application-architecture)
- [Another Web Framework?](#another-web-framework)
- [Technical Details](#technical-details)
- [Contributing](#contributing)
- [Name](#name)

# Introduction
*Minimum viable product. Not production ready.*

Telescope is a Haskell framework for building reactive web apps, fast. Telescope
abstracts away common tasks you undertake when developing a web app, *allowing
you to focus on your business logic* and *reducing the time you need to build
your app*.

An application built with Telescope is..
- **Reactive:** don't worry about keeping client-side and server-side data in
  sync, with Telescope your *frontend can automatically react to changes in your
  database* and your database can be updated seamlessly by your frontend!
- **Robust:** writing the strongly-typed language Haskell across the stack
  prevents server/client protocol mismatches and other run-time errors, allowing
  you to *move fast and not break things.*
- **Minimal:** Telescope can setup a database and server for you and also manage
  communication between client and server, so you can *focus on the parts of
  your application that really matter.*

What doesn't Telescope do currently?
- Provide a full-featured database query language.
- Generate a small file to be sent to web clients.

Telescope is particularly well-suited for applications where events are pushed
by the server e.g. notifications and dashboards. Telescope also handles forms
and input-validation very well. On the flip-side, applications with heavy
client-side computation such as animations are not well-suited for Telescope.

## Getting Started
Building a reactive web app with Telescope looks a little like this:

*1.* Declare the data types used in your application.

``` haskell
data TodoList = TodoList {
    name  :: String
  , items :: [String]
  } deriving (Generic, Show)

instance PrimaryKey TodoList where
  primaryKey = name
```

*2.* Populate your database with some data.

``` haskell
T.set $ TodoList "pancakes" ["eggs", "milk", "flour"]
```

*3.* Start the Telescope server.

``` haskell
Server.run port
```

*4.* Write the frontend of your reactive web app with
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

*5.* Open two tabs, edit the to-do list in one tab and watch the other react!

``` haskell
T.over $ TodoList{} "pancakes" (++ ["sugar", "lemon juice"])
```

A full tutorial and demo application are available TODO.
<!-- TODO: links to reflex-platform and other doc in demo/README.md -->

## Application Architecture
<!-- Core is the Telescope interface, available client & server-side. -->
The most important component of Telescope from an application developer's
perspective is the Telescope interface, a set of functions that allow you to
read/write datatypes from/to a data source. This interface is available both
server-side and client-side. The diagram below shows one possible setup of a
Telescope application, each telescope icon represents usage of the Telescope
interface.

<!-- Bottom row. -->
The bottom row of the diagram represents a developer interacting with a database
on their own machine. More specifically the developer has opened a REPL and is
using the Telescope interface to interact with the local database.

<!-- Top row, server is a proxy. -->
The top row of the diagram shows two uses of the Telescope interface, one by a
server, and one by a web client. The server is interacting with the database via
the Telescope interface and acts as a proxy to the database for any web clients.
The web client is communicating with the server via the Telescope interface, but
since the server is only acting as a proxy the client is really interacting with
the database.

<div align="center">
  <img src="diagram/diagram.png" />
</div>

There are a number of important points about the Telescope interface which we
will go over in turn.
- There are reactive variants of Telescope functions
- Multiple instances of the Telescope interface
- The Telescope interface is data source agnostic
- Data is uniquely determined by type and primary key
<!-- TODO: finish discussion about these points. -->
<!-- TODO: outline data flow, top row subscribes and reacts to data. -->

## Another Web Framework?
<!-- Many existing frameworks, pros and cons. -->
There are many different web frameworks out there, and they all have pros and
cons. They pretty much all allow you to write reuseable components. Some can
ship a small file to the client, some allow you to write your server-side and
client-side code in the same language, some can pre-render server-side for a
speedy TTI (time to interactive).

<!-- Reactive frontend is popular. -->
One idea that has become fairly popular in recent years is that of a reactive
frontend. Whereby the frontend is written as a function of the current state,
and whenever the state changes, the frontend "reacts" to the change and updates
itself.

<!-- Network is boundary of reactivity. -->
Implementations of reactive frontends vary, however in the vast majority of
cases one significant limitation is that the frontend only reacts to client-side
changes in data. At this network boundary the developer still has to manage
communication with a server.

<!-- Liberated of where/when. -->
The primary motivation behind creating Telescope is that a developer should be
able to *write a reactive frontend as a function of data in their one true data
source*. Telescope solves this by providing a direct Reflex-DOM to database
link. Even better, the Telescope interface is not specific to Reflex-DOM (where
the interface is used) or the database. You could write an instance to use in
e.g. a `reflex-vty` application, or to communicate with a different server or
database.

## Technical Details 
<!-- Reactive interface to data, data location is a parameter. -->
So Telescope is a web framework? More generally Telescope provides a reactive
interface (the `Telescope` typeclass) to read/write datatypes from/to a data
source. The data source's location (e.g. filepath or URL) is a parameter of the
interface, allowing you to use the same interface to read/write data regardless
of where that data is stored. Even better, you can use the Telescope interface
both server-side and client-side. The use of the term "reactive" in "reactive
interface" refers to the fact that clients can subscribe to changes in data,
reacting to any changes.

<!-- Various packages. -->
The `telescope` package provides the Telescope interface without any instances.
The `telescope-ds-file` package provides an instance of the interface, that
stores data in local files. The `telescope-ds-reflex-dom` package provides an
instance of the interface to be used in a [Reflex-DOM](https://reflex-frp.org/)
web app, it talks to a server to read/write data. The `telescope-server` package
provides a [Servant](https://www.servant.dev/) server that serves data via a
provided instance of the Telescope interface e.g. from `telescope-ds-file`.

<!-- Generic programming. -->
The Telescope interface provides functions that operate on datatypes which are
instances of the `Entity` typeclass. You only need to define a primary key for
your datatype and then the `Entity` instance can be derived for your datatype
via `Generics`. Generic programming allows conversion of your datatype to/from
storable representation. The following diagram shows this conversion.

``` haskell
-- Example of a datatype to be stored.
data Person { name :: String , age  :: Int } deriving Generic
instance PrimaryKey Person where primaryKey = name
  
-- Diagram showing conversion to/from storable representation.
Person "john" 70     <--->     "Person"
                               | ID     | "name" | "age" |
                               | "john" | "john" | 70    |
```

<!-- TODO: example that results in two rows. -->

## Contributing
Install the [Nix](https://nixos.org/download.html) package manager, clone this
repository (with submodules) and change in to the `telescope` directory. If
you’ve never built a project with `reflex-platform` before you'll also need to
run a configuration step. Note that initial builds will take a looong time.

``` bash
curl -L https://nixos.org/nix/install | sh
git clone --recurse-submodules https://github.com/jerbaroo/telescope
cd telescope
./reflex-platform/try-reflex  # Configure reflex-platform.
```

Development commands for the Telescope framework:

``` bash
# Type-check the package passed as first argument.
./scripts/check.sh telescope
# Run tests for all Telescope packages.
./scripts/test.sh
# Start a Hoogle server (optional port argument).
./scripts/hoogle.sh 5000
```

Development commands for the demo application:

``` bash
# Run a server for demo-backend, server restarts on file change.
./scripts/run/dev/backend.sh
# Run a server for demo-frontend, server restarts on file change.
./scripts/run/dev/frontend.sh
# Enter a REPL for interacting with demo database.
./scripts/repl.sh
```

Production commands for the demo application:

``` bash 
# Build the demo-backend server.
./scripts/build/prod/backend.sh 
# Generate demo-frontend static files.
./scripts/build/prod/frontend.sh 
# Run a server for demo-backend.
./scripts/run/prod/backend.sh
```

## Name
The `telescope` package provides an interface to read/write remote data i.e.
data stored in a database or data accessed over the network. This interface is
"lens-like" i.e. the functions are similar to the functions `view`, `set` etc.
that you may know from the `lens` library. So if you squint your eyes a little
you could say this library provides a lens to look at remote data... like a
telescope.
