{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import           Database.Beam
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
import           Prelude

import           Data.Text              (Text)

(#) :: a -> (a -> b) -> b
a # f = f a

{- |
type family Columnar (f :: * -> *) x where ...

Equations
  Columnar Exposed x = Exposed x
  Columnar Identity x = x
  Columnar (Lenses t f) x = LensFor (t f) (Columnar f x)
  Columnar (Nullable c) x = Columnar c (Maybe x)
  Columnar f x = f x
-}

-- Tables
data UserT f
  = User
  { _userEmail     :: Columnar f Text
  , _userFirstName :: Columnar f Text
  , _userLastName  :: Columnar f Text
  , _userPassword  :: Columnar f Text
  }
  deriving Generic
instance Beamable UserT

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
  primaryKey = UserId . _userEmail
instance Beamable (PrimaryKey UserT)

newtype ShoppingCartDb f
  = ShoppingCartDb
  { _shoppingCartUsers :: f (TableEntity UserT)
  }
  deriving Generic

instance Database be ShoppingCartDb

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings

main :: IO ()
main = do
  connection <- open "db/shoppingcart1.db"
  -- runBeamSqliteDebug putStrLn connection $
  --   runInsert
  --   . insert (_shoppingCartUsers shoppingCartDb)
  --   . insertValues $
  --     [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c"
  --     , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f"
  --     , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c"
  --     ]

  let allUsers = all_ (_shoppingCartUsers shoppingCartDb)
  runBeamSqliteDebug putStrLn connection $ do
    users <- runSelectReturningList $ select allUsers
    mapM_ (liftIO . print) users

  let sortUsersByFirstName =
        all_ (_shoppingCartUsers shoppingCartDb)
        # orderBy_
            (\user ->
              ( asc_ (_userFirstName user)
              , desc_ (_userLastName user)
              )
            )
  runBeamSqliteDebug putStrLn connection $ do
    users <- runSelectReturningList $ select sortUsersByFirstName
    mapM_ (liftIO . print) users

  let boundedQuery :: Q SqliteSelectSyntax _ _ _
      boundedQuery =
          all_ (_shoppingCartUsers shoppingCartDb)
          # orderBy_ (asc_ . _userFirstName)
          # offset_ 1
          # limit_ 1
  runBeamSqliteDebug putStrLn connection $ do
    users <- runSelectReturningList (select boundedQuery :: SqlSelect SqliteSelectSyntax _)
    mapM_ (liftIO . print) users
