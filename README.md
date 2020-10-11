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

## Typical Workflow

Building a reactive web app with Telescope looks something like this:

**1.** Declare the data types used in your application, and derive (don't write)
instances for these data types via `Generics` so Telescope understands them.

``` haskell
data TodoList = TodoList {
    name :: String
  , items :: [String]
  } deriving (Generic, Show)

instance PrimaryKey TodoList where
  primaryKey = name
```

**2:** Populate your database with some dummy data to test your app.

**3:** Start a server and test it serves your data as expected.

**4:** Write the frontend of you reactive web app!

## Motivation

About React. Function from data to frontend. DRY.

## A Little Clarification

Some terms have been bandied about without real clarification, so I hope to
clear up a few things in this section. However the best way to learn is by
doing, so I suggest jumping to the tutorial TODO and building your first
Telescope web app, and coming back to this document later.

"[lens](https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html)-like":
Telescope provides functions to manipulate data in a data source, these look
similar to the functions `view`, `set` etc. in the lens library. The reason for
this is that manipulating "remote" data (e.g. on a server or in a database)
should be as easy and consistent with the way you manipulate data locally in
your program with the lens library.

"reactive": reactive variants of the Telescope functions to manipulate data are
provided (`viewX` instead of `view`). So your [Reflex](https://reflex-frp.org/)
web app can stay up-to-date with the data in your database. And it goes both
ways: you can stream updates to your database with `setX` as a user edits an
input field!

"data source": the functions are data source agnostic, currently you can access
data either from local file-based storage or over the internet.

Goals and anti-goals: TODO
- "library-integration"

TODO: Technical Diagram

## Typical Usage

Here are a few snippets of how you might use Telescope in a Servant
(server-side) and Reflex (client-side) application. For a more in-depth
explanation of `telescope-lib` see its [Tutorial](src/Telescope/Tutorial.hs)
module, or look at the README files for any of the other `telescope-` packages.

First here are some data types you might have in an application.

```haskell
data User = User {
    uEmail    :: Text
  , uPassword :: Text
  , uStatus   :: Status
}

data Status = Unverified | Verifed
```

We need to make these data types instances of `Storable` so they work with
Telescope. If we provide instances for `Generic` and `PKey` that is enough, a
`Storable` instance will be derived. The `PKey` typeclass declares a primary key
for a data type.

```haskell
data User = User {
    uEmail    :: Text
  , uPassword :: Text
  , uStatus   :: Status
} deriving (Generic)

instance PKey User where
  pKey = uEmail

data Status = Unverified | Verifed deriving (Generic)
```

We can operate on the data in local file-based storage with the
`telescope-source-file` package.

```haskell
config <- fileConfig "example.db"
let user = User "john@gmail.com" "1234" Unverified
set user config
print =<< view user config
```

We can start a server to serve the data types from a specified data source with
the `telescope-server` package.

```haskell
config <- fileConfig "example.db"
server <- telescopeServer "localhost" 5000 "secret" config
runServer server
```

Visit *localhost:5000/user/view/john@gmail.com* in a browser to verify the
server is working.

Data can be accessed by a Reflex application in the same way as is done
server-side, with the `telescope-reflex` package.

```haskell
config <- fileConfig "example.db"
print =<< view user config
```

When starting a server you can also specify an existing server as the data
source. Thus the new server acts as a proxy.

```haskell
config <- networkConfig "localhost" 5000 "secret"
proxyServer <- telescopeServer "localhost" 5001 "new-secret" config
runServer proxyServer
```
