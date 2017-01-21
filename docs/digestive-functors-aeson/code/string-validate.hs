{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Text.Digestive
import Text.Digestive.Aeson
import Data.Scientific
import Data.Maybe
import Data.Aeson hiding ((.:), Result, Error, Success)
import Prelude hiding (Result)

-- In apis that communicate using JSON, we often have to validate
-- an incoming json payload, before we pass it into our application.
-- Tradition json parsing falls short in a couple of ways

-- 1. It cannot collect more than one error
-- 2. It cannot add validation to the successfully parsed JSON

-- Digestive functors aeson, is a lib that try to solve this problem.

-- A json 'Value' can be validated using the `digestJSON` function.
-- 
--     digestJSON :: Monad m => Form v m a	-> Value	-> m (View v, Maybe a)	 
-- 
-- The inputs to this function is a value of type 'Form v m a' and the actual
-- JSON 'Value' type (exported by Data.Aeson)
-- 
-- This library provides functions to create those, the simplest of which are
--     text, string and stringRead functions.

-- 
--     type Form v m a = FormTree m v m a 
-- 
-- The three type parameters are:
-- 
-- v: the type for error message. Usually Text or String
-- m: the monad in which validators operate. For starters, this can be IO
-- a: The data type that we want the json to be converted to

-- Api summary.
--  Digestive functor library provides functions to create the Form values
--  for String, Text and Instance of Show/Read. 
--
--  Once you have a Form, you can 
--  use the .: function to read the corresponding typed from a json key. For ex
--  If you have a form that can read an Integer, you can make a form that will
--  read an integer from the "age" filed using 
--     "age" .: formInt
--  Once you have a form that reads the value of type a, you can add validation check
--  to the form using the "check" function. This function accepts a Form of type `a`
--  and function `a -> Bool` and a validation message and returns another Form of
--  the same type, but with validation added.

--  Then you apply the Forms thus created to Aeson values using the digestJSON function
--  to obtain the successfully parsed and validated value, along with a "View' that
--  contains validation messages.

--  Those are the basics and let us see some examples.

-- Note: We will be validating types of 'Value' which is a data type exported by the Aeson json parsing libray.

-- here are a couple of functions that will create values of type Value.
stringValue :: Value
stringValue = String (pack "A String")

intWrappedInString :: Value
intWrappedInString = fromJust $ decode "\"25\""

intValue :: Value
intValue = fromJust $ decode "25"

-- here is a Value that represents a string in an object
wrappedStringValue :: Value
wrappedStringValue = stringValue'
  where
  Just stringValue' = decode "{\"InputString\": \"A string\"}"

-- Here we create a form that can read (but not validate) a String. 
-- We use the `string` function from digestive functors library.
-- The Maybe argument is a default value to return, incase the validation fails.
stringForm :: Form Text IO String
stringForm = string Nothing

-- Here is a form for reading a String that do one validation
stringFormWithValidation :: Form Text IO String
stringFormWithValidation = check "String need to be at least 10 chars long" testLength $ string Nothing
    where
      testLength :: String -> Bool
      testLength s = Prelude.length s >= 10

-- Here is a form that reads a string from a key "InputString" from a json Object
-- {"InputString" : "A String"}
stringFormValidationFromKey :: Form Text IO String
stringFormValidationFromKey = "InputString" .: stringForm

-- Here is a form that reads a user defined datatype from the same json as above
data UserName = UserName String
stringFormValidationFromKeyToUserDefined :: Form Text IO UserName
stringFormValidationFromKeyToUserDefined = UserName <$> "InputString" .: stringForm

-- PARSING INTEGERS AND SUCH
-- Here is a form that read an integer that is embedded as a String in the json.
-- For ex: {"age" : "25"}
numberFromStringForm :: Form Text IO Int
numberFromStringForm = stringRead "Failed to parse as a number" Nothing
--
-- Please note in the above that the number 25 is wrapped in quotes.
-- If you want to read from unwrapped numeric values.
-- For ex: {"age" : 25}, You have to first create
-- a form that can read a Scientific value, and then use the functor instance to
-- make forms for other number types from it. Here is how you would read an unwrapped integer
numberFromNumberForm :: Form Text IO Int
numberFromNumberForm = validate checkAndConvertToInt scientificForm
  where
    checkAndConvertToInt :: Scientific -> Result Text Int
    checkAndConvertToInt s = if isInteger s then Success (floor s) else Error "Number is not an intger"
    scientificForm :: Form Text IO Scientific
    scientificForm = stringRead "Failed to parse as a number" Nothing

-- Here is how you can collect validation failures from multiple validations on the 
-- same form.

-- Below is a form for reading a String that do two validation
-- That collect all the validation errors
stringFormWithTwoValidation :: Form [Text] IO String
stringFormWithTwoValidation = 
      validate (conditions [testLength, testNoSpace]) $
      string Nothing
    where
      testLength :: String -> Result Text String
      testLength s = if Prelude.length s < 10 then Error (pack "String should be longer than 10 chars") else Success s
      testNoSpace :: String -> Result Text String
      testNoSpace s = if elem ' ' s then Error (pack "String should not contain a space") else Success s

testStringForm :: IO ()
testStringForm = do
  validationResult <- digestJSON stringForm stringValue
  putStrLn $ show validationResult
  validationResult <- digestJSON stringFormWithValidation stringValue
  putStrLn $ show validationResult
  validationResult <- digestJSON stringFormValidationFromKey wrappedStringValue
  putStrLn $ show validationResult

main = return ()
