{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Aeson
import Data.Text
import GHC.Generics

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--
data Operator
  = Equals
  | SomethingElse
  deriving (Show, Generic)

instance FromJSON Operator

instance ToJSON Operator

testDecoding :: Either String Operator
testDecoding = eitherDecode "\"Equals\""

--
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

class Record a where
  getValue :: FieldName -> a -> Maybe Field

data Field
  = TextField Text
  | IntegerField Integer
  | BoolField Bool
  deriving (Show)

type FieldName = Text

instance Record Person where
  getValue "name" = Just . wrap . name
  getValue "age" = Just . wrap . age
  getValue _ = const Nothing

instance Record Pet where
  getValue "name" = Just . wrap . petName
  getValue "dangerous" = Just . wrap . dangerous

match :: (FieldValue a, Record r) => FieldName -> a -> r -> Bool
match field expected record =
  case wrapped >>= unwrap of
    Just value -> value == expected
    Nothing -> False
  where
    wrapped = getValue field record

class Eq a =>
      FieldValue a where
  wrap :: a -> Field
  unwrap :: Field -> Maybe a

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

-- what about using Aeson?
getValue' :: FieldName -> Person -> Value
getValue' "name" = String . name
