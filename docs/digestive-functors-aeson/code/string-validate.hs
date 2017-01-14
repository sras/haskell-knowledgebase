{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Text.Digestive
import Text.Digestive.Aeson
import Data.Aeson hiding ((.:))

-- A json 'Value' can be validated using the `digestJSON` function.
-- 
--     digestJSON :: Monad m	 => Form v m a	-> Value	-> m (View v, Maybe a)	 
-- 
-- The first argument is most important data type here, which is
-- 
--     type Form v m a = FormTree m v m a 
-- 
-- The three type parameters are:
-- 
-- v: the type for error message. Usually Text or String
-- m: the monad in which validators operate. For starters, this can be IO
-- a: The data type that we want the json to be converted to
-- 
-- Note: We will be validating types of 'Value' which is a data type exported by the Aeson json parsing libray.

-- here are a couple of functions that will create values of type Value.
stringValue :: Value
stringValue = stringValue'
  where
  Just stringValue' = decode "\"A string\""

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

testStringForm :: IO ()
testStringForm = do
  validationResult <- digestJSON stringForm stringValue
  putStrLn $ show validationResult
  validationResult <- digestJSON stringFormWithValidation stringValue
  putStrLn $ show validationResult
  validationResult <- digestJSON stringFormValidationFromKey wrappedStringValue
  putStrLn $ show validationResult

main = return ()
