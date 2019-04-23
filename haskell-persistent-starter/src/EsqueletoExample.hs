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
, ScopedTypeVariables
, StandaloneDeriving
, TemplateHaskell
, TypeFamilies
#-}
module EsqueletoExample (example) where

import Prelude

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader (..))
import Data.Foldable (traverse_)
import Database.Esqueleto
import Database.Persist.MySQL (MySQLConnectInfo, withMySQLConn, setMySQLConnectInfoPort, mkMySQLConnectInfo)
import Database.Persist.TH
import EsqueletoExample.Blog (runBlogT)

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
      ] [persistLowerCase|
Person
  name String
  age Int Maybe
  deriving Eq Show
BlogPost
  title String
  authorId PersonId
  deriving Eq Show
Follow
  follower PersonId
  followed PersonId
  deriving Eq Show
|]

{-
SELECT *
FROM Person
-}
putPersons :: MonadIO m => SqlPersistT m ()
putPersons = do
  people
    <- select
      $ from \person -> pure person
  liftIO $ traverse_ (putStrLn . ("Name: " ++) . personName . entityVal) people

{-
SELECT *
FROM Person
WHERE Person.name = "John"
-}
getJohns :: MonadIO m => SqlReadT m [Entity Person]
getJohns =
  select
  $ from \p -> do
      where_ (p ^. PersonName ==. val "John")
      pure p

{-
SELECT *
FROM Person
WHERE Person.age >= 18
-}
getAdults :: MonadIO m => SqlReadT m [Entity Person]
getAdults =
  select
  $ from \p -> do
      where_ (p ^. PersonAge >=. just (val 18))
      pure p

{-
SELECT BlogPost.*, Person.*
FROM BlogPost, Person
WHERE BlogPost.authorId = Person.id
ORDER BY BlogPost.title ASC
-}
getBlogPostsByAuthors :: MonadIO m => SqlReadT m [(Entity BlogPost, Entity Person)]
getBlogPostsByAuthors =
  select
  $ from \(b, p) -> do
      where_ (b ^. BlogPostAuthorId ==. p ^. PersonId)
      orderBy [asc (b ^. BlogPostTitle)]
      pure (b, p)

{-
SELECT Person.*, BlogPost.*
FROM Person
  LEFT OUTER JOIN BlogPost
  ON Person.id = BlogPost.authorId
ORDER BY Person.name ASC, BlogPost.title ASC
-}
getAutorMaybePosts :: MonadIO m => SqlReadT m [(Entity Person, Maybe (Entity BlogPost))]
getAutorMaybePosts =
  select
  $ from \(p `LeftOuterJoin` mb) -> do
      on (just (p ^. PersonId) ==. mb ?. BlogPostAuthorId)
      orderBy [ asc (p ^. PersonName)
              , asc (mb ?. BlogPostTitle)
              ]
      pure (p, mb)

getMutualFollowers :: MonadIO m => SqlReadT m [(Entity Person, Entity Follow, Entity Person)]
getMutualFollowers =
  select
  $ from \(p1 `InnerJoin` f `InnerJoin` p2) -> do
      on (p2 ^. PersonId ==. f ^. FollowFollowed)
      on (p1 ^. PersonId ==. f ^. FollowFollower)
      pure (p1, f, p2)

updatePerson :: MonadIO m => SqlWriteT m ()
updatePerson =
  update \p -> do
    set p [ PersonName =. val "wenbo"]
    where_ (p ^. PersonName ==. val "robot")


deleteYoungsters :: MonadIO m => SqlPersistT m ()
deleteYoungsters = do
  youngsters
    <- select
      $ from \p -> do
          where_ (p ^. PersonAge <. just (val 14))
          pure p
  forM_ youngsters (deleteKey . entityKey) -- deleteCascade

deleteYoungsters' :: MonadIO m => SqlPersistT m ()
deleteYoungsters' =
  delete
  $ from \p -> do
      where_ (p ^. PersonAge <. just (val 14))

{-
INSERT INTO BlogPost
  SELECT ('Init post for everyone.', id)
  FROM Person
-}
insertBlogPosts :: MonadIO m => SqlWriteT m ()
insertBlogPosts =
  insertSelect
  $ from \p ->
      return $ BlogPost <# (val "Init post for everyone.") <&> (p ^. PersonId)

setupDb :: MonadIO m => SqlPersistT m ()
setupDb = do
  -- | Run migrations and create the test database entries
  runMigration migrateAll
  createDb
  where
    createDb :: MonadIO m => SqlPersistT m ()
    createDb = do
      john <- insert $ Person "John" (Just 24)
      sean <- insert $ Person "Sean" (Just 70)
      joao <- insert $ Person "Joao" (Just 13)
      void $ insertMany [ BlogPost "How to play a bodhran" sean
                        , BlogPost "Haskell for the working class hero" john
                        ]
      void $ insert $ Follow john sean
      void $ insert $ Follow sean john
      void $ insert $ Follow joao sean

maxConnectionNum :: Int
maxConnectionNum = 10

-- runDb
--   :: forall (m :: * -> *) a
--   . ( MonadReader MySQLConnectInfo m
--     , MonadIO m
--     , MonadBaseControl IO m
--     , MonadLogger m
--     )
--   => SqlPersistT m a
--   -> m a
runDb
  :: forall (m :: * -> *) a
  . ( MonadLogger m
    , MonadReader MySQLConnectInfo m
    , MonadUnliftIO m
    )
  => SqlPersistT m a
  -> m a
runDb query = do
  connInfo <- ask
  -- withMySQLPool connInfo maxConnectionNum $ \pool ->
  --   liftIO $ flip runSqlPersistMPool pool $ do
  --     printMigration migrateAll
  --     query
  withMySQLConn connInfo \backend -> do
    runSqlConn query backend


connectionInfo :: MySQLConnectInfo
connectionInfo =
  setMySQLConnectInfoPort 3300
  $ mkMySQLConnectInfo "127.0.0.1" "root" "root" "persistent"

example :: IO ()
example = runBlogT connectionInfo . runDb $ do
  log ""
  log "DB Migration ---------------------"
  setupDb

  log ""
  log "All Persons ---------------------"
  putPersons

  log ""
  log "getJohns ---------------------"
  johns <- getJohns
  mapM_ say johns

  log ""
  log "getAdults ---------------------"
  adults <- getAdults
  mapM_ say adults

  log ""
  log "getBlogPostsByAuthors ---------------------"
  authorBlogPosts <- getBlogPostsByAuthors
  mapM_ say authorBlogPosts

  log ""
  log "getAutorMaybePosts ---------------------"
  authorMaybePosts <- getAutorMaybePosts
  mapM_ say authorMaybePosts

  log ""
  log "getMutualFollowers ---------------------"
  mutualFollowers <- getMutualFollowers
  mapM_ say mutualFollowers

  -- insertBlogPosts

  where
    say :: (MonadIO m, Show a) => a -> m ()
    say  = liftIO . print

    log :: MonadIO m => String -> m ()
    log = liftIO . putStrLn

