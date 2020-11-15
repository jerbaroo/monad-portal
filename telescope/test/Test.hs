{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main ( main ) where

import           Data.Text                 ( Text )
import qualified Data.Map                 as Map
import           GHC.Generics              ( Generic )
import           Telescope.Class           ( PrimaryKey(..) )
import           Telescope.Convert        as Convert
import           Telescope.Storable.To    as Storable
import           Telescope.Storable.Types as Storable
import           Telescope.Table.Types    as Table
import qualified Test.HUnit               as HUnit
import qualified System.Exit              as Exit

main :: IO ()
main = do
  results <- HUnit.runTestTT $ HUnit.TestList
    [ testPrims
    , testMaybe
    , testList
    ]
  if   HUnit.errors results + HUnit.failures results == 0
  then Exit.exitWith   Exit.ExitSuccess
  else Exit.exitWith $ Exit.ExitFailure 1

-- | Data type used to test primitive values.
data Person = Person { name :: Text, age :: Int, cycles :: Bool }
  deriving (Eq, Generic, Show)

instance PrimaryKey Person Text where primaryKey = name

testPrims :: HUnit.Test
testPrims = HUnit.TestCase $ do
  -- Conversion of primitives to storable representation.
  let john = Person { name = "John", age = 70, cycles = True }
      johnStorable = Storable.SDataType
        ( Table.TableKey "Person"
        , Table.RowKey $ Table.KeyOne $ Table.PText "John"
        )
        $ Storable.SFields
          [ (Table.ColumnKey "name"  , Storable.SValuePrim (Table.PText "John"))
          , (Table.ColumnKey "age"   , Storable.SValuePrim (Table.PInt  70    ))
          , (Table.ColumnKey "cycles", Storable.SValuePrim (Table.PBool True  ))
          ]
  HUnit.assertEqual "To storable representation of primitives failed"
    johnStorable $ Storable.toSDataType john

  -- Conversion of primitives to table representation.
  let johnTable = Map.fromList
        [ ( Table.TableKey "Person"
          , Map.fromList
            [ ( Table.RowKey $ Table.KeyOne $ Table.PText "John"
              , [ (Table.ColumnKey "name"  , PText "John")
                , (Table.ColumnKey "age"   , PInt  70    )
                , (Table.ColumnKey "cycles", PBool True  )
                ]
              )
            ]
          )
        ]
  HUnit.assertEqual "To table representation of primitives failed"
    johnTable $ Convert.aToRows john

  -- Reconstruction of primitives from table representation.
  let johnRow = johnTable Map.! Table.TableKey "Person" Map.!
        (Table.RowKey $ Table.KeyOne $ Table.PText "John")
  HUnit.assertEqual "Primitives reconstruction from table representation failed"
    john $ Convert.aFromRow johnRow

-- | Data type used to test 'Maybe' values.
data May = May { be :: Maybe Int, foo :: Int } deriving (Eq, Generic, Show)

instance PrimaryKey May Int where primaryKey = foo

testMaybe :: HUnit.Test
testMaybe = HUnit.TestCase $ do
  -- Conversion of 'Just' to storable representation.
  let just = May { be = Just 21, foo = 70 }
      justStorable = Storable.SDataType
        (Table.TableKey "May" , Table.RowKey $ Table.KeyOne $ Table.PInt 70)
        $ Storable.SFields
          [ (Table.ColumnKey "be" , Storable.SValuePrim $ Table.PInt 21)
          , (Table.ColumnKey "foo", Storable.SValuePrim $ Table.PInt 70)
          ]
  HUnit.assertEqual "To storable representation of 'Just' failed"
    justStorable $ Storable.toSDataType just

  -- Conversion of 'Nothing' to storable representation.
  let nothing = May { be = Nothing, foo = 70 }
      nothingStorable = Storable.SDataType
        (Table.TableKey "May" , Table.RowKey $ Table.KeyOne $ Table.PInt 70)
        $ Storable.SFields
          [ (Table.ColumnKey "be" , Storable.SValuePrim $ Table.PNull  )
          , (Table.ColumnKey "foo", Storable.SValuePrim $ Table.PInt 70)
          ]
  HUnit.assertEqual "To storable representation of 'Nothing' failed"
    nothingStorable $ Storable.toSDataType nothing

  -- Conversion of 'Just' to table representation.
  let justTable = Map.fromList
        [ ( Table.TableKey "May"
          , Map.fromList
            [ ( Table.RowKey $ Table.KeyOne $ Table.PInt 70
              , [ (Table.ColumnKey "be" , PInt 21)
                , (Table.ColumnKey "foo", PInt 70)
                ]
              )
            ]
          )
        ]
  HUnit.assertEqual "To table representation of 'Just' failed"
    justTable $ Convert.aToRows just

  -- Conversion of 'Nothing' to table representation.
  let nothingTable = Map.fromList
        [ ( Table.TableKey "May"
          , Map.fromList
            [ ( Table.RowKey $ Table.KeyOne $ Table.PInt 70
              , [ (Table.ColumnKey "be" , PNull  )
                , (Table.ColumnKey "foo", PInt 70)
                ]
              )
            ]
          )
        ]
  HUnit.assertEqual "To table representation of 'Nothing' failed"
    nothingTable $ Convert.aToRows nothing

  -- Reconstruction of 'Just' from table representation.
  let justRow = justTable Map.! Table.TableKey "May" Map.!
        (Table.RowKey $ Table.KeyOne $ Table.PInt 70)
  HUnit.assertEqual "'Just' reconstruction from table representation failed"
    just $ Convert.aFromRow justRow

  -- Reconstruction of 'Nothing' from table representation.
  let nothingRow = nothingTable Map.! Table.TableKey "May" Map.!
        (Table.RowKey $ Table.KeyOne $ Table.PInt 70)
  HUnit.assertEqual "'Nothing' reconstruction from table representation failed"
    nothing $ Convert.aFromRow nothingRow

-- | Data type used to test lists of primitives.
data List = List { bar :: Int, car :: [Int]}
  deriving (Eq, Generic, Show)

instance PrimaryKey List Int where primaryKey = bar

testList :: HUnit.Test
testList = HUnit.TestCase $ do
  -- Conversion of list to storable representation.
  let list = List 1 [2, 3]
      listStorable = Storable.SDataType
        ( Table.TableKey "List"
        , Table.RowKey $ Table.KeyOne $ Table.PInt 1
        )
        $ Storable.SFields
          [ (Table.ColumnKey "bar", Storable.SValuePrim (Table.PInt  1                ))
          , (Table.ColumnKey "car", Storable.SValuePrim (Table.PText "[PInt 2,PInt 3]"))
          ]
  HUnit.assertEqual "To storable representation of list failed"
    listStorable $ Storable.toSDataType list

  -- Conversion of list to table representation.
  let listTable = Map.fromList
        [ ( Table.TableKey "List"
          , Map.fromList
            [ ( Table.RowKey $ Table.KeyOne $ Table.PInt 1
              , [ (Table.ColumnKey "bar", PInt 1                 )
                , (Table.ColumnKey "car", PText "[PInt 2,PInt 3]")
                ]
              )
            ]
          )
        ]
  HUnit.assertEqual "To table representation of list failed"
    listTable $ Convert.aToRows list

  -- Reconstruction of 'Nothing' from table representation.
  let listRow = listTable Map.! Table.TableKey "List" Map.!
        (Table.RowKey $ Table.KeyOne $ Table.PInt 1)
  HUnit.assertEqual "List reconstruction from table representation failed"
    list $ Convert.aFromRow listRow
