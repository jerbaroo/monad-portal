# Telescope

**Not production ready.**

Telescope helps you build reactive web apps with Haskell, fast. Telescope
achieves this is by abstracting away common tasks when developing a web app,
**allowing you to focus on your data** and **reducing the time you need to build
a reactive web app**

What can Telescope do for you?
- You don't need to setup a database, Telescope can handle this for you.
- You don't need to setup a server, Telescope can handle this for you.
- You can write the same language (Haskell) server-side and client-side.
- Server-side and client-side code use the same functions for database access!
- You don't have to worry about keep client-side and server-side data in sync.
- Your frontend will automatically react to changes in your database!

What is Telescope not?
- A full featured database access library e.g. SQL.

## Typical Workflow

Building a reactive web app with Telescope looks something like this:

**1.** Declare the data types used in your application, derive the necessary
`Generic` instances and define a primary key for your data types.

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
set $ TodoList "pancakes" ["eggs", "milk", "flour"]
```

**3:** Start a server and check it is serving your data as expected.

**4:** Write the frontend of your reactive web app!

## Motivation

There are many different web frameworks out there, and they all have pros and
cons. They pretty much all allow you to write reuseable components. Some have to
ship a large filesize to the client, some allow you to write your server-side
and client-side code in the same language, some can pre-render server-side for
a speedy TTI (time to interactive) score.

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

TODO: Add comparison table between popular frameworks.

## Development

Install the [Nix](https://nixos.org/download.html) package manager, clone this
repository (with submodules) and change in to the `telescope` directory:

``` bash
curl -L https://nixos.org/nix/install | sh
git clone --recurse-submodules https://github.com:jerbaroo/telescope
cd telescope
```

Useful commands for developing the Telescope framework:

``` bash
# Enter a shell with dependencies installed.
nix-shell -A shells.ghc
# Build and test all Telescope packages.
./build-test.sh
# Run GHCID for the telescope package.
(cd telescope && ./ghcid.sh)
# Run a Telescope server executable.
./run-server.sh
```

Useful commands for developing the demo application:

``` bash
# Build with Nix & Cabal (supports incremental build). 
nix-shell -A shells.ghc --run "cabal new-build demo-frontend"
# Build with Nix & GHCJS.
nix-build -o demo-frontend-ghcjs -A ghcjs.demo-frontend
```

## Name

Before this framework was able to setup a database or a server, it was really
just a data access library. Providing functions to access remote data i.e. data
stored in a database, or accessing data over the network. These functions are
"lens-like", they are similar to the functions `view`, `set` etc. that you may
know from the `lens` library. So you could say the library provides a lens to
look at remote data... like a telescope.
