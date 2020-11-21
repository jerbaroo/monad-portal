{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | The important user-facing type classes.
module Telescope.Class (module Telescope.Class, Table.PrimaryKey(..)) where

import           Control.Comonad          ( Comonad )
import           Control.Exception        ( throw )
import           Data.Functor.Identity    ( Identity(Identity) )
import qualified Data.Map                as Map
import           Data.Set                 ( Set )
import qualified Data.Set                as Set
import qualified Telescope.Exception     as Exc
import           Telescope.Storable.From  ( FromSValues )
import           Telescope.Storable.To    ( ToSDataType )
import qualified Telescope.Table.To      as Table
import qualified Telescope.Table.Types   as Table

-- | A storable data type (can be serialized and deserialized via Generics).
type Entity a k = (ToSDataType a k, FromSValues a)

-- | 'Table.Row'-based operations for interacting with a data source.
--
-- Instances of this type class are the engines behind 'Telescope.Operations'.
--
-- This class has two type variables, 'm' and 'f'. 'm' is the monad which the
-- operations will run in. 'f' is a container type, which all input and output
-- values will be wrapped in. The type variable 'f' enables this type class to
-- support both synchronous and reactive 'Telescope' instances. For instances of
-- this type class that support the synchronous API, 'f' might be the 'Identity'
-- functor. However for instances that support a reactive API (e.g. an instance
-- used in a Reflex-DOM application) 'f' might represent a stream of values.
class (Functor f, Monad m) => Telescope m f | m -> f where

  -- | View one row in a data source.
  viewRow :: f Table.Ref -> m (f (Maybe Table.Row))
  viewRow keysF = do
    rowsF <- viewRows $
      (\(tk, rk) -> Map.singleton tk $ Set.singleton rk) <$> keysF
    updateMaybe (toMaybeRow <$> rowsF) $ watchRow keysF
    where toMaybeRow :: Table.Tables -> Maybe Table.Row
          toMaybeRow tables =
            case Map.elems tables of
              []      -> Nothing
              [table] -> case Map.elems table of
                []    -> Nothing
                [row] -> Just row
                _     -> throw $ Exc.InstanceException
                  "viewRow: too many rows returned by 'viewRows'"
              _       -> throw $ Exc.InstanceException
                  "viewRow: too many tables returned by 'viewRows'"

  -- | View multiple rows in a data source.
  viewRows :: f Table.RowsIndex -> m (f (Table.Tables))

  -- | View one table in a data source.
  viewTable :: f Table.TableKey -> m (f Table.Table)
  viewTable tableKeyF = do
    tablesF <- viewTables $ Set.singleton <$> tableKeyF
    pure $ toTable <$> tablesF
    where toTable :: Table.Tables -> Table.Table
          toTable tables =
            case Map.elems tables of
              []      -> Map.empty
              [table] -> table
              _       -> throw $ Exc.InstanceException
                "viewTable: too many tables returned by 'viewTables'"

  -- | View multiple tables in a data source.
  viewTables :: f (Set Table.TableKey) -> m (f Table.Tables)

  -- | Set one row in a data source.
  setRow :: f (Table.TableKey, Table.RowKey, Table.Row) -> m ()
  setRow keysF =
    setRows $ (\(tk, rk, r) -> Map.singleton tk $ Map.singleton rk r) <$> keysF

  -- | Set multiple rows in a data source.
  setRows :: f Table.Tables -> m ()

  -- | Set one table in a data source.
  setTable :: f (Table.TableKey, Table.Table) -> m ()
  setTable keyAndTableF = setTables $ uncurry Map.singleton <$> keyAndTableF

  -- | Set multiple tables in a data source.
  setTables :: f Table.Tables -> m ()

  -- | Remove one row from a data source.
  rmRow :: f Table.Ref -> m ()
  rmRow keysF =
    rmRows $ (\(tk, rk) -> Map.singleton tk $ Set.singleton rk) <$> keysF

  -- | Remove multiple rows from a data source.
  rmRows :: f Table.RowsIndex -> m ()

  -- | Remove one table from a data source.
  rmTable :: f Table.TableKey -> m ()
  rmTable tableKeyF = rmTables $ Set.singleton <$> tableKeyF

  -- | Remove multiples table from a data source.
  rmTables :: f (Set Table.TableKey) -> m ()
  rmTables tableKeysF = setTables $ Map.fromSet (const Map.empty) <$> tableKeysF

  ------------------------------------------------------------------------------
  -- Functions for watching a data source for changes --------------------------
  ------------------------------------------------------------------------------

  -- | If updates can be pushed to the container type 'f' (e.g. 'Reflex.Event')
  -- then this must be set to 'True' and 'update' must be implemented.
  updateable :: m Bool

  -- | Update the first argument with any changes that occur in the second.
  update :: f a -> f a -> m (f a)
  update _ _ = throw $ Exc.InstanceException
    "'updateable' returned 'True' but 'update' is not implemented"

  -- | If this container is 'updateable' then evaluate the second argument and
  -- send any updates to the first argument via 'update'. If the container is
  -- not 'updateable' then the first agument will simply be returned.
  updateMaybe :: f a -> m (f a) -> m (f a)
  updateMaybe aF changesFM = updateable >>= \case
    False -> pure aF
    True  -> update aF =<< changesFM

  -- | Run a function when a row in a data source changes.
  onChangeRow :: f Table.Ref -> f (Maybe Table.Row -> m ()) -> m ()

  -- | Watch one row in a data source for changes.
  watchRow :: f Table.Ref -> m (f (Maybe Table.Row))

-- | A container type for synchronous 'Telescope' instances.
class Comonad f => Box f where
  box :: a -> f a

-- | A container type readily-available for synchronous 'Telescope' intances.
instance Box Identity where
  box = Identity

-- | A poor performing default implementation of 'viewRows' using 'Applicative'.
viewRowsCheap :: (Applicative f, Telescope m f)
  => f Table.RowsIndex -> m (f (Table.Tables))
viewRowsCheap rowKeysMapF = do
  tablesF <- viewTables $ Set.fromList . Map.keys <$> rowKeysMapF
  pure $ Map.intersectionWith Map.restrictKeys <$> tablesF <*> rowKeysMapF

-- | A poor performing default implementation of 'setRows' using 'Applicative'.
setRowsCheap :: (Applicative f, Telescope m f) => f Table.Tables -> m ()
setRowsCheap newRowsMapF = do
  tablesF <- viewTables $ Set.fromList . Map.keys <$> newRowsMapF
  setTables $ Map.unionWith Map.union <$> newRowsMapF <*> tablesF

-- | A poor performing default implementation of 'rmRows' using 'Applicative'.
rmRowsCheap :: (Applicative f, Telescope m f) => f Table.RowsIndex -> m ()
rmRowsCheap rowKeysMapF = do
    tablesF <- viewTables $ Set.fromList . Map.keys <$> rowKeysMapF
    setTables $ Map.intersectionWith Map.withoutKeys <$> tablesF <*> rowKeysMapF
