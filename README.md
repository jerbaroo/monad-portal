# MonadPortal 

Consistent interface to data across server and client with minimal boilerplate.

MonadPortal derives a generic representation of your data types via `Generics`
and provides a set of tools to enable communication between your DB, web server
and web client in terms of these generic values. The interface supports both
regular and streaming clients.

``` haskell
DB <-- generic data --> Server <-- generic data --> Client
```

## Introduction

`MonadPortal` is an interface to read/write data from/to an arbitrary data
source. The structure of the data in the interface's methods is generic allowing
for both "regular" (single value per call) or streaming clients (e.g. Reflex).

``` haskell
-- | A monad that can communicate with an arbitrary data source.
--
-- 'f' is the shape of the data, for example 'f' could be the Identity functor
-- or a value that can change over time (e.g. 'Dynamic' from Reflex).
class MonadPortal m f | m -> f where
  read :: ...
```

To use `MonadPortal` with your data types, you just to implement `PrimaryKey`:

``` haskell
data Person { name :: Text, age  :: Int } deriving Generic

instance PrimaryKey Person Text where
  primaryKey = name
  
john <- read @Person "John"
```

You can plug any data source you like into a `MonadPortal`. We provide these
helpers so hopefully you don't have to write a `MonadPortal` instance:
- a `MonadPortal` instance for Reflex apps which interacts with a HTTP server.
- a Servant HTTP server that serves data via a provided `MonadPortal` instance.
- a `MonadPortal` instance that communicates with a PostgreSQL database.

## Getting Started

See the `examples` folder for working examples.

Example projects use reflex-platform (Nix) to provide dependencies, because
Reflex is used as the frontend library in all examples. Start by setting up the
reflex-platform binary cache on your operating system, and then skim the
reflex-platform project development guide:
- [setting up reflex-platform on your OS](https://github.com/reflex-frp/reflex-platform?tab=readme-ov-file#os-compatibility)
- [reflex-platform project development guide](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst)

## Generics

Here we define a simple data type and show the generic representations we
derive. Note that we first derive an intermediary generic representation, and
then convert that into a final "flattened" representation.

``` haskell
data User = User
  { name :: Text
  , age :: Int
  , child :: Maybe User
  } deriving Generic

instance PrimaryKey User Text where
  primaryKey = name

john :: User
john = User "John" 21 Nothing

mary :: User
mary = User "Mary" 51 $ Just john
```

### Intermediate Representation

``` haskell
SDataType
    (
        ( TableKey "User"
        , RowKey ( PrimText "Mary" ) []
        )
    , SFields
        [
            ( ColumnKey "name"
            , SValuePrim
                ( PrimNotNull ( PrimText "Mary" ) )
            )
        ,
            ( ColumnKey "age"
            , SValuePrim
                ( PrimNotNull ( PrimInt 51 ) )
            )
        ,
            ( ColumnKey "child"
            , SValueDataType
                ( SDataType
                    (
                        ( TableKey "User"
                        , RowKey ( PrimText "John" ) []
                        )
                    , SFields
                        [
                            ( ColumnKey "name"
                            , SValuePrim
                                ( PrimNotNull ( PrimText "John" ) )
                            )
                        ,
                            ( ColumnKey "age"
                            , SValuePrim
                                ( PrimNotNull ( PrimInt 21 ) )
                            )
                        ,
                            ( ColumnKey "child"
                            , SValuePrim PrimNull
                            )
                        ]
                    )
                )
            )
        ]
    )
```

### Flattened Representation

``` haskell
fromList
    [
        ( TableKey "User"
        , fromList
            [
                ( RowKey ( PrimText "John" ) []
                ,
                    [
                        ( ColumnKey "name"
                        , PrimNotNull ( PrimText "John" )
                        )
                    ,
                        ( ColumnKey "age"
                        , PrimNotNull ( PrimInt 21 )
                        )
                    ,
                        ( ColumnKey "child"
                        , PrimNull
                        )
                    ]
                )
            ,
                ( RowKey ( PrimText "Mary" ) []
                ,
                    [
                        ( ColumnKey "name"
                        , PrimNotNull ( PrimText "Mary" )
                        )
                    ,
                        ( ColumnKey "age"
                        , PrimNotNull ( PrimInt 51 )
                        )
                    ,
                        ( ColumnKey "child"
                        , PrimNotNull
                            ( PrimRef
                                ( TableKey "User"
                                , RowKey ( PrimText "John" ) []
                                )
                            )
                        )
                    ]
                )
            ]
        )
    ]
```

## Limitations

What are MonadPortal's limitations?
- MonadPortal does not provide a full-featured database query language.
- Automatic derivation of schemas only works for data types with:
  - a single constructor
  - all fields named
- The server exported by MonadPortal does not currently support authorization
  checks. **High priority**.

## Developing

To work on this repo start by setting up reflex-platform on your OS and reading
the reflex-plaftorm project development guide, links to both above in the
Getting Started section.

``` bash
ghcid -c 'cabal new-repl' telescope
```
