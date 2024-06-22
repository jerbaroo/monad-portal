{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Portal.Class where

import           Control.Comonad          ( Comonad )
import           Control.Exception        ( throw )
import           Data.Functor.Identity    ( Identity(Identity) )
import qualified Data.Map                as Map
import           Data.Set                 ( Set )
import qualified Data.Set                as Set
import qualified Data.Portal.Exception     as Exc
import           Data.Portal.Storable.From  ( FromSValues )
import           Data.Portal.Storable.To    ( ToSDataType )
import qualified Data.Portal.Table.Types   as Table

-- | Storable types (can be serialized and deserialized).
-- TODO move to Data.Portal.Storable.Class
type Entity a k = (ToSDataType a k, FromSValues a)

-- | Table representation-based operations for interacting with a data source.
--
-- Instances of this typeclass are the engines behind 'Data.Portal.Operations'.
--
-- This typeclass has two type variables, 'm' and 'f'. 'm' is the monad that
-- operations will run in. 'f' is a container type, which all input and output
-- values will be wrapped in.
class (Functor f, Monad m) => MonadPortal m f | m -> f where

  -- | View one row in a data source.
  viewRow :: f Table.Ref -> m (f (Maybe Table.Row))
  viewRow refF = do
    rowsF <- viewRows $
      (\(tk, rk) -> Map.singleton tk $ Set.singleton rk) <$> refF
    updateMaybe (toMaybeRow <$> rowsF) $ watchRow refF
    where toMaybeRow :: Table.Rows -> Maybe Table.Row
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
  viewRows :: f Table.RowKeys -> m (f (Table.Rows))

  -- | View one table in a data source.
  viewTable :: f Table.TableKey -> m (f Table.Table)
  viewTable tableKeyF = do
    tablesF <- viewTables $ Set.singleton <$> tableKeyF
    updateMaybe (toTable <$> tablesF) $ watchTable tableKeyF
    where toTable :: Table.Rows -> Table.Table
          toTable tables =
            case Map.elems tables of
              []      -> Map.empty
              [table] -> table
              _       -> throw $ Exc.InstanceException
                "viewTable: too many tables returned by 'viewTables'"

  -- | View multiple tables in a data source.
  viewTables :: f (Set Table.TableKey) -> m (f Table.Rows)

  -- | Set one row in a data source.
  setRow :: f (Table.TableKey, Table.RowKey, Table.Row) -> m ()
  setRow refF =
    setRows $ (\(tk, rk, r) -> Map.singleton tk $ Map.singleton rk r) <$> refF

  -- | Set multiple rows in a data source.
  setRows :: f Table.Rows -> m ()

  -- | Set one table in a data source.
  setTable :: f (Table.TableKey, Table.Table) -> m ()
  setTable keyAndTableF = setTables $ uncurry Map.singleton <$> keyAndTableF

  -- | Set multiple tables in a data source.
  setTables :: f Table.Rows -> m ()

  -- | Remove one row from a data source.
  rmRow :: f Table.Ref -> m ()
  rmRow refF =
    rmRows $ (\(tk, rk) -> Map.singleton tk $ Set.singleton rk) <$> refF

  -- | Remove multiple rows from a data source.
  rmRows :: f Table.RowKeys -> m ()

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

  -- | Run a function when a row in a data source changes.
  onChangeTable :: f Table.TableKey -> f (Table.Table -> m ()) -> m ()

  -- | Watch one row in a data source for changes.
  watchRow :: f Table.Ref -> m (f (Maybe Table.Row))

  -- | Watch on table in a data source for changes.
  watchTable :: f Table.TableKey -> m (f Table.Table)

-- | A container for synchronous 'MonadPortal' instances.
class Comonad f => Box f where
  box :: a -> f a

-- | A container readily-available for synchronous 'MonadPortal' intances.
instance Box Identity where
  box = Identity

-- | A poor performing default implementation of 'viewRows' using 'Applicative'.
viewRowsCheap :: (Applicative f, MonadPortal m f)
  => f Table.RowKeys -> m (f (Table.Rows))
viewRowsCheap rowKeysMapF = do
  tablesF <- viewTables $ Set.fromList . Map.keys <$> rowKeysMapF
  pure $ Map.intersectionWith Map.restrictKeys <$> tablesF <*> rowKeysMapF

-- | A poor performing default implementation of 'setRows' using 'Applicative'.
setRowsCheap :: (Applicative f, MonadPortal m f) => f Table.Rows -> m ()
setRowsCheap newRowsMapF = do
  tablesF <- viewTables $ Set.fromList . Map.keys <$> newRowsMapF
  setTables $ Map.unionWith Map.union <$> newRowsMapF <*> tablesF

-- | A poor performing default implementation of 'rmRows' using 'Applicative'.
rmRowsCheap :: (Applicative f, MonadPortal m f) => f Table.RowKeys -> m ()
rmRowsCheap rowKeysMapF = do
    tablesF <- viewTables $ Set.fromList . Map.keys <$> rowKeysMapF
    setTables $ Map.intersectionWith Map.withoutKeys <$> tablesF <*> rowKeysMapF
