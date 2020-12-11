{-# LANGUAGE MonoLocalBinds #-}

{-| Convert data types to and from table representation.

Two internal representations of a data type exist, "table" and "storable". A
data type is converted to table representation when being written to a data
source, and reconstructed from table representation when reading from a data
source. The storable representation is an intermediate representation between
the data type and table representation:

@data type <--> storable representation <--> table representation@

The difference between storable and table representations is that in table
representation everything is flattened into rows of primitives. In storable
representation, a data type consists of the same primitives, but the data type
is not yet flattened into rows (a data type can contain nested storable data
types). Table representation is defined in "Telescope.Table.Types" and storable
representation in "Telescope.Storable.Types".

@data Person { fname :: Text, parent :: Maybe Person }@
@instance PrimaryKey Person Text where primaryKey = fname
@somePerson = Person "John" $ Person "Michael" Nothing@

In pseudo-storable representation:

"Person": {
  { "fname" : "John"
  , "parent": "Person": {"fname": "Michael": "parent": PNull}
  }

In pseudo-table representation:

"Person"
| "fname"   | "parent"       |
|-----------|----------------|
| "John"    | FKey "Michael" |
| "Michael" | PNull          |

Some terminology:
  - @"Person"@ is the type.
  - @"fname"@ is a field's name.
  - @"John"@ is a field's value.
-}
module Telescope.Convert where

import qualified Telescope.Storable.To   as Storable
import qualified Telescope.Storable.From as Storable
import qualified Telescope.Table.To      as Table
import qualified Telescope.Table.From    as Table
import qualified Telescope.Table.Types   as Table

-- | Convert a data type to table representation.
aToRows :: Storable.ToSDataType a k => a -> Table.Tables
aToRows = Table.sToRows . Storable.toSDataType

-- | A data type reconstructed from table representation.
aFromRow :: Storable.FromSValues a => Table.Row -> a
aFromRow = Storable.fromSValues . Table.rowToSValues
