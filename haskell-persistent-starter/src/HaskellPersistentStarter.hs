{-# LANGUAGE
  BlockArguments
, DeriveDataTypeable
, DeriveFoldable
, DeriveFunctor
, DeriveGeneric
, DeriveTraversable
, ExistentialQuantification
, ExplicitForAll
, FunctionalDependencies
, FlexibleInstances
, FlexibleContexts
, GADTs
, GeneralizedNewtypeDeriving
, MultiParamTypeClasses
, NamedFieldPuns
, NoImplicitPrelude
, OverloadedStrings
, QuasiQuotes
, RankNTypes
, StandaloneDeriving
, ScopedTypeVariables
, TemplateHaskell
, TypeFamilies
#-}

module HaskellPersistentStarter
( test
) where

import Prelude

import Database.Persist.TH (mkPersist, mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.MySQL (SqlBackend, MySQLConnectInfo, mkMySQLConnectInfo, setMySQLConnectInfoPort, withMySQLPool, runSqlPersistMPool, SelectOpt(..), runMigration)
import Database.Persist
import Control.Category ((<<<))
import Control.Monad.Logger (NoLoggingT, runStderrLoggingT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)

-- Utils
(#) :: a -> (a -> b) -> b
x # f = f x

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show

BlogPost
    title String
    authorId PersonId
    deriving Show
|]

connectionInfo :: MySQLConnectInfo
connectionInfo =
  mkMySQLConnectInfo "127.0.0.1" "root" "root" "persistent"
  # setMySQLConnectInfoPort 3300

maxOpenConnectionCount :: Int
maxOpenConnectionCount = 10

test :: IO ()
test =
  runStderrLoggingT
  <<< withMySQLPool connectionInfo maxOpenConnectionCount
    $ \pool ->
      liftIO
      <<< flip runSqlPersistMPool pool
        $ go

  where
    -- NOTE https://hackage.haskell.org/package/resourcet-1.2.2/docs/Control-Monad-Trans-Resource.html#t:MonadUnliftIO
    go :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
    go = do
      runMigration migrateAll

      johnId <- insert $ Person "John Doe" (Just 25)
      insert_ $ Person "Jane Doe" Nothing

      insert_ $ BlogPost "My first post" johnId
      insert_ $ BlogPost "One more for good measure" johnId
      insert_ $ BlogPost "sad" johnId

      oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 2]
      liftIO $ print (oneJohnPost :: [Entity BlogPost])

      john <- get johnId
      liftIO $ print (john :: Maybe Person)


