{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib where

import Data.Text

{- Value is a "wrapper" for raw values; this enables us to have functions
that return different types without breaking type-safety -}
data Value
  = TextValue Text
  | IntegerValue Integer
  | BoolValue Bool
  deriving (Show)

{- FromValue is any type that can be unwrapped from a Value back to its raw
form; since we usually unwrap values without knowing its internal type,
we can't be certain the unwrapping will be successful - ie. if we try to
unwrap a TextValue in an area where a Bool is necessary, the operation can
fail; to account for that possibility, unwrap has to return a Maybe -}
class Eq a =>
      ValueWrappable a where
  wrap :: a -> Value
  unwrap :: Value -> Maybe a

{- Here we "teach" Text, Integer and Bool values to unwrap themselves from a
Value by defining instances of FromValue -}
instance ValueWrappable Text where
  wrap = TextValue
  unwrap (TextValue v) = Just v
  unwrap _ = Nothing

instance ValueWrappable Integer where
  wrap = IntegerValue
  unwrap (IntegerValue v) = Just v
  unwrap _ = Nothing

instance ValueWrappable Bool where
  wrap = BoolValue
  unwrap (BoolValue v) = Just v
  unwrap _ = Nothing

{- Record is anything that contains data that is "gettable" by FieldName;
since a Record can have multiple fields with different types, the "getValue"
function has to return a Field. -}
class Record r f where
  getValue :: f -> r -> Value

{- Some sample data types and convenience constructors... -}
data Person = Person
  { name :: Text
  , age :: Integer
  } deriving (Show)

data PersonField
  = Name
  | Age
  deriving (Eq)

samplePerson :: Person
samplePerson = Person "Thiago" 35

data Pet = Pet
  { petName :: Text
  , dangerous :: Bool
  } deriving (Show)

data PetField
  = PetName
  | Dangerous
  deriving (Eq)

samplePet :: Pet
samplePet = Pet "Frankie" False

{- Here we define instances of Record for Person and Pet, "teaching" them
how to fetch values based on field names -}
instance Record Person PersonField where
  getValue Name = wrap . name
  getValue Age = wrap . age

instance Record Pet PetField where
  getValue PetName = wrap . petName
  getValue Dangerous = wrap . dangerous

{- This is where the magic happens! Given a field, an expected value and
a record, this returns True if the actual value on that field matches the
expected or False otherwise. It also returns False if the given field name
does not exist or if the type of the expected parameter doesn't match the
type of the actual value on the record -}
satisfies :: (ValueWrappable v, Record r f) => f -> v -> r -> Bool
satisfies field expected record =
  case unwrap $ getValue field record of
    Just value -> value == expected
    Nothing -> False
