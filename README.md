# Telescope

**In development. Not production ready.**

Telescope helps you build reactive web apps with Haskell, fast. Telescope
achieves this by abstracting away common tasks you need to undertake when
developing a web app, **allowing you to focus on your data** and **reducing the
time you need to build your app**.

With Telescope..
- you don't need to setup a database, Telescope can handle this for yoU.
- you don't need to setup a server, Telescope can handle this for you.
- you can write the same language (Haskell) server-side and client-side.
- server-side and client-side code use the same functions for database access!
- you don't have to worry about keeping client-side and server-side data in sync.
- your frontend will automatically react to changes in your database!

What is Telescope not?
- A full featured database access library e.g. SQL.

## Typical Workflow

Building a reactive web app with Telescope looks something like this:

**1.** Declare the data types used in your application.

``` haskell
data TodoList = TodoList {
    name  :: String
  , items :: [String]
  } deriving (Generic, Show)

instance PrimaryKey TodoList where
  primaryKey = name
```

**2:** Populate your database with some dummy data so you can test your app.

``` haskell
T.set $ TodoList "pancakes" ["eggs", "milk", "flour"]
```

**3:** Start the Telescope server (or integrate it with your existing Server).

``` haskell
Server.run port
```

**4:** Write the frontend of your reactive web app!

``` haskell
-- NOTE: work in progress.
main = mainWidget $ el "div" $ do
  el "h3" $ text "View a table:"
  input   <- textInput def
  results <- T.view (constant TodoList{}) $ updated $ input ^. textInput_value
  dynText =<< (holdDyn "No results." $ (pack . show) <$> results)
```

A full tutorial and demo application are available TODO.
<!-- TODO: links to reflex-platform and other doc in demo/README.md -->

The following diagram shows the typical setup of a Telescope application. The
database and server are setup by Telescope with minimal work by a user. The
server, and a local user using a terminal, are communicating with the database
via the Telescope interface. Finally a client is communicating with the database
through the server, note that the client's communication with the server is with
the same Telescope interface.

<p align="center">
  <img src="diagram/diagram.png" />
</p>

## Motivation

There are many different web frameworks out there, and they all have pros and
cons. They pretty much all allow you to write reuseable components. Some have to
ship a large filesize to the client, some allow you to write your server-side
and client-side code in the same language, some can pre-render server-side for
a speedy TTI (time to interactive).

One idea that has become fairly popular in recent years is that of a reactive
frontend. Whereby the view is written as a function of the current state, and
whenever the state changes, the view "reacts" to the change and is updated.

Implementations of reactive frontends vary, one possible implementation is
"data-binding", available as one-way in React and two-way in Polymer. With this
implementation the data "flows" up/down your tree of components. With React you
still have to do some work in order to update parent nodes because the binding
is only one-way (parent to child). A solution to this problem with React is to
introduce /another library/ Redux which separates state from your components.

Polymer and Vue provide more elegant solutions that allow you to write your
frontend as a function of your application state. However, in both cases you
still have to manage the communication between client and server. The primary
motivation behind the Telescope framework is that your frontend is simply a
function of state, and a developer should be able to write a function from app
state to view liberated from concerns of where the data is stored or how the
view is updated. In other words Telescope solves the DRY problem, there is only
one source of data, and how it is accessed, server-side or client-side, is the
exact same.

<!-- TODO: Add comparison table between popular frameworks. -->

## Development

Install the [Nix](https://nixos.org/download.html) package manager, clone this
repository (with submodules) and change in to the `telescope` directory. If
you’ve never built a project with `reflex-platform` before you'll also need to
run a configuration step.

``` bash
curl -L https://nixos.org/nix/install | sh
git clone --recurse-submodules https://github.com:jerbaroo/telescope
cd telescope
./reflex-platform/try-reflex # Configure reflex-platform.
```

To run the demo application:

``` bash
# Runs a server with a database containing some dummy data.
./scripts/run-server.sh
# Builds the frontend with Nix & GHC and starts a frontend server.
# Once the server is running visit localhost:3003 in a browser.
./scripts/run-frontend.sh
```

Commands for developing the demo application:

``` bash
# Builds the backend with Cabal & GHC.
./scripts/build-backend.sh
# Builds the frontend with Nix & GHCJS (producing an index.html file).
./scripts/build-frontend.sh
```

Commands for developing the Telescope framework:

``` bash
# Builds all Telescope packages.
./scripts/build-telescope.sh
# Runs tests for Telescope packages.
./scripts/test-telescope.sh
# Runs GHCID for the telescope package.
./scripts/ghcid.sh telescope
```

## Name

Before this framework was able to setup a database or a server, it was really
just a data access library. Providing functions to access remote data i.e. data
stored in a database, or accessing data over the network. These functions are
"lens-like", they are similar to the functions `view`, `set` etc. that you may
know from the `lens` library. So you could say the library provides a lens to
look at remote data... like a telescope.
