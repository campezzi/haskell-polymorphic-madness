{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Text

{- FieldName is just an alias to make signatures more readable -}
type FieldName = Text

{- Field is a "wrapper" for raw values; this enables us to have functions
that return different types without breaking type-safety -}
data Field
  = TextField Text
  | IntegerField Integer
  | BoolField Bool
  deriving (Show)

{- FieldValue is any data type that can be wrapped as a Field and unwrapped
back to its primitive value; this allows us to wrap and unwrap primitive types
without knowing what the actual types are, relying on the type system to
dispatch "wrap" and "unwrap" calls to the right instances -}
class Eq a =>
      FieldValue a where
  wrap :: a -> Field
  unwrap :: Field -> Maybe a

{- Here we "teach" Text, Integer and Bool values to wrap and unwrap
themselves in/from a Field by creating instances of FieldValue -}
instance FieldValue Text where
  wrap = TextField
  unwrap (TextField v) = Just v
  unwrap _ = Nothing

instance FieldValue Integer where
  wrap = IntegerField
  unwrap (IntegerField v) = Just v
  unwrap _ = Nothing

instance FieldValue Bool where
  wrap = BoolField
  unwrap (BoolField v) = Just v
  unwrap _ = Nothing

{- Record is anything that contains data that is "gettable" by field name;
since a Record can have multiple fields with different types, the "getValue"
function has to return a Field. Also, because there may not be a field with
that name on the Record, this Field has to be wrapped in a Maybe -}
class Record a where
  getValue :: FieldName -> a -> Maybe Field

{- Some sample data types and convenience constructors... -}
data Person = Person
  { name :: Text
  , age :: Integer
  } deriving (Show)

data Pet = Pet
  { petName :: Text
  , dangerous :: Bool
  } deriving (Show)

samplePerson :: Person
samplePerson = Person "Thiago" 35

samplePet :: Pet
samplePet = Pet "Frankie" False

{- Here we define instances of Record for Person and Pet, "teaching" them
how to fetch values based on field names -}
instance Record Person where
  getValue "name" = Just . wrap . name
  getValue "age" = Just . wrap . age
  getValue _ = const Nothing

instance Record Pet where
  getValue "name" = Just . wrap . petName
  getValue "dangerous" = Just . wrap . dangerous

{- This is where the magic happens! Given a field name, an expected value and
a record, this returns True if the actual value on that field matches the
expected or False otherwise. It also returns False if the given field name
does not exist or if the type of the expected parameter doesn't match the
type of the actual value on the record -}
match :: (FieldValue a, Record r) => FieldName -> a -> r -> Bool
match field expected record =
  case wrapped >>= unwrap of
    Just value -> value == expected
    Nothing -> False
  where
    wrapped = getValue field record
