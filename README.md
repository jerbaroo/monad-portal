# MonadData 

MonadData allows client and server to interface with the same data source.

``` haskell
DB <--MonadData--> Server <--MonadData--> Client
```

MonadData is particularly useful for full-stack Haskell applications where
events are pushed by the server to the client e.g. live data dashboards.

## Introduction

MonadData provides an interface to read/write data from/to an arbitrary data
source. The structure of the data in the interface's methods is generic (e.g.
could be a single datum per call or a stream of data):

``` haskell
-- | A monad that can interface with a data source.
--
-- `f` is the shape of the data, for example `f` could be the Identity functor
-- or a value that can change over time (e.g. `Dynamic` from Reflex).
class MonadData m f | m -> f where

-- | Read from a data source.
read :: (Data a k, MonadData m f) => f a -> m (f (Maybe a))
```

Once you have an instance of `MonadData` you only need to make your data type an
instance of `Data` in order to read/write values of that type from/to your data
source:

``` haskell
data Person { name :: Text, age  :: Int } deriving Generic

instance Data Person Text where
  primaryKey = "name"
  
john <- read @Person "John"
```

Once you have a client-side `MonadData` instance to communicate with your
server, and a server-side `MonadData` instance to communicate with your DB, then
effectively your server is merely acting as a proxy to your DB for your client!

We provide a few helpers so you can avoid boilerplate:
- a Servant server `Application`. Merely supply a `MonadData` instance so the
  server knows how to communicate with your data source.
- a `MonadData` instance for Reflex clients, which communicates with the
  endpoints of the Servant server `Application` which we provide.
- a `MonadData` instance which communicates with a PostgreSQL database, you can
  plug this into the Servant server `Application` which we provide.

## Example

See the `examples` folder for working examples.

Example projects use reflex-platform (Nix) to provide dependencies, because
Reflex is used as the frontend library in all examples. Start by setting up the
reflex-platform binary cache on your operating system, and then skim the
reflex-platform project development guide:
- [setting up reflex-platform on your OS](https://github.com/reflex-frp/reflex-platform?tab=readme-ov-file#os-compatibility)
- [reflex-platform project development guide](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst)

## FAQ

How does MonadData work?
- MonadData derives a mapping between data type and schema via `Generics`.

What are MonadData's limitations?
- MonadData does not provide a full-featured database query language.
- Serialization of data types is not as efficient as it could be.
- The server exported by MonadData does not currently support authorization
  checks. **High priority**.
