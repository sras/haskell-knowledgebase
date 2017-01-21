{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE Arrows        #-}

module Main where

import Opaleye
import GHC.Int (Int64)
import Data.Profunctor.Product
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH           (makeAdaptorAndInstance)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField(..))

import Prelude hiding (id)
import Control.Arrow

newtype UserId = UserId Int deriving (Show)

data UserPoly id name email = User { id :: id, name :: name, email :: email } deriving (Show)

type User = UserPoly UserId String String
type UserPGW = UserPoly (Column PGInt4) (Column PGText) (Column PGText)
type UserPGR = UserPoly (Column PGInt4) (Column PGText) (Column PGText)
type UserPGR1 = UserPoly (Column PGInt4) (Column (Nullable PGText)) (Column PGText)

$(makeAdaptorAndInstance "pUser" ''UserPoly)

userTable :: Table UserPGW UserPGR
userTable = Table "users" (pUser User {
    id = required "id",
    name = required "name",
    email = required "email"
    }
  )

instance FromField UserId where
  fromField field bs = UserId <$> fromField field bs

instance QueryRunnerColumnDefault PGInt4 UserId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

getUserRows :: IO [UserPoly Int (Maybe String) String]
getUserRows = do
  conn <- connect defaultConnectInfo { connectDatabase = "scratch"}
  runQuery conn $ query2
  where
    query :: Opaleye.Query UserPGR
    query = queryTable userTable
    query2 :: Opaleye.Query UserPGR1
    query2 = proc () -> do
      User id name email <- query -< ()
      returnA -<  User id Opaleye.null email

main :: IO ()
main = do
  rows <- getUserRows
  putStrLn $ show rows

-- Output
-- [User {id = UserId 1, name = "John", email = "john@mail.com"},User {id = UserId
--  2, name = "Bob", email = "bob@mail.com"},User {id = UserId 3, name = "Alice",
-- email = "alice@mail.com"}]
