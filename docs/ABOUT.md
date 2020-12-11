# About Telescope

## Application Architecture
The most important component of Telescope from an application developer's
perspective is the Telescope interface, a set of functions that allow you to
read/write datatypes from/to a data source. This interface is available both
server-side and client-side. The diagram below shows one possible setup of a
Telescope application, each telescope icon represents usage of the Telescope
interface.

The bottom row of the diagram represents a developer interacting with a database
on their own machine. More specifically the developer has opened a REPL and is
using the Telescope interface to interact with the local database.

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
will now discuss in turn, referring back to the architecture diagram above.
- Reactive variants of Telescope functions.
- Multiple instances of the Telescope interface.
- The Telescope interface is data source agnostic.
- Data is uniquely determined by type and primary key.

## Another Web Framework?
There are many different web frameworks out there, and they all have pros and
cons. They pretty much all allow you to write reuseable components. Some can
ship a small file to the client, some allow you to write your server-side and
client-side code in the same language, some can pre-render server-side for a
speedy TTI (time to interactive).

One idea that has become fairly popular in recent years is that of a reactive
frontend. Whereby the frontend is written as a function of the current state,
and whenever the state changes, the frontend "reacts" to the change and updates
itself.

Implementations of reactive frontends vary, however in the vast majority of
cases one significant limitation is that the frontend only reacts to client-side
changes in data. At this network boundary the developer still has to manage
communication with a server.

The primary motivation behind creating Telescope is that a developer should be
able to **write a reactive frontend as a function of data in their one true data
source**. Telescope solves this by providing a direct Reflex-DOM <-> database
link. Even better, the Telescope interface is not specific to Reflex-DOM or the
database. You could write an instance to use in e.g. a `reflex-vty` application,
or to communicate with a different server or database.

## Technical Details 
So Telescope is a web framework? More generally Telescope provides a reactive
interface (the `Telescope` typeclass) to read/write datatypes from/to a data
source. The data source's location (e.g. filepath or URL) is a parameter of the
interface, allowing you to use the same interface to read/write data regardless
of where that data is stored. Even better, you can use the Telescope interface
both server-side and client-side. The use of the term "reactive" in "reactive
interface" refers to the fact that clients can subscribe to changes in data,
reacting to any changes.

The `telescope` package provides the Telescope interface without any instances.
The `telescope-ds-file` package provides an instance of the interface, that
stores data in local files. The `telescope-ds-reflex-dom` package provides an
instance of the interface to be used in a [Reflex-DOM](https://reflex-frp.org/)
web app, it talks to a server to read/write data. The `telescope-server` package
provides a [Servant](https://www.servant.dev/) server that serves data via a
provided instance of the Telescope interface e.g. from `telescope-ds-file`.

The Telescope interface provides functions that operate on datatypes which are
instances of the `Entity` typeclass. You only need to define a primary key for
your datatype and then the `Entity` instance can be derived for your datatype
via `Generics`. Generic programming allows conversion of your datatype to/from
storable representation. The following diagram shows this conversion.

``` haskell
-- Example of a datatype to be stored.
data Person { name :: Text, age  :: Int } deriving Generic
instance PrimaryKey Person where primaryKey = name
  
-- Diagram showing conversion to/from storable representation.
Person "john" 70     <--->     "Person"
                               | ID     | "name" | "age" |
                               | "john" | "john" | 70    |
```

