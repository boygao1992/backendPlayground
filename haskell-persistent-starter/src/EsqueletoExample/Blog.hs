module EsqueletoExample.Blog
( runBlogT
) where

import Prelude

import Control.Monad.Logger (MonadLogger, NoLoggingT (..))
import Control.Monad.Reader
import Database.Persist.MySQL (MySQLConnectInfo)
import Control.Monad.IO.Unlift (UnliftIO(..), MonadUnliftIO, withUnliftIO, askUnliftIO)


newtype BlogT m a = BlogT
  { unBlogT :: NoLoggingT (ReaderT MySQLConnectInfo m) a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadLogger
           , MonadReader MySQLConnectInfo
           , MonadIO
           )

instance MonadTrans BlogT where
  lift = BlogT . lift . lift

-- instance MonadTransControl BlogT where
--   type StT BlogT a = StT NoLoggingT (StT (ReaderT MySQLConnectInfo) a)
--   liftWith f =
--     BlogT $ liftWith $ \run ->
--       liftWith $ \run' ->
--         f (run' . run . unBlogT)

--   restoreT = BlogT . restoreT . restoreT

-- instance MonadBaseControl b m => MonadBaseControl b (BlogT m) where
--   type StM (BlogT m) a = ComposeSt BlogT m a
--   liftBaseWith = defaultLiftBaseWith
--   restoreM = defaultRestoreM

{- NOTE MonadUnliftIO
class Monad m => MonadIO m where
  liftIO :: IO a -> m a

class MonadIO m => UnliftIO m where
  askUnliftIO :: m (UnliftIO m) -- m (forall a. m a -> IO a)
  withRunInIO :: ((forall a. m a -> IO a) -> IO b) -> m b

newtype UnliftIO m = UnliftIO { unliftIO :: forall a. m a -> IO a }

withUnliftIO :: MonadUnliftIO m => (UnliftIO m -> IO a) -> m a
                  -- askUnliftIO :: m (UnliftIO m)
withUnliftIO inner = askUnliftIO >>= liftIO . inner
                                  -- liftIO . inner :: UnliftIO m -> m a

newtype IdentityT m a = IdentityT { runIdentityT :: m a }
instance MonadUnliftIO m => MonadUnliftIO (IdentityT m) where
  askUnliftIO :: IdentityT m (UnliftIO (IdentityT m))
  askUnliftIO =
    IdentityT
                 -- u :: UnliftIO m
    $ withUnliftIO \u ->
                       -- unliftIO u :: m ~> IO
        pure $ UnliftIO $ unliftIO u . runIdentityT
                                    -- runIdentityT :: IdentityT m ~> m
                       -- unliftIO u . runIdentityT :: IdentityT m ~> IO
            -- UnliftIO $ unliftIO u . runIdentityT :: UnliftIO (IdentityT m)
     -- pure $ UnliftIO $ unliftIO u . runIdentityT :: m (UnliftIO (IdentityT m))

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
instance MonadUnliftIO m => MonadUnliftIO (ReaderT r m) where
  askUnliftIO :: ReaderT r m (UnliftIO (ReaderT r m))
  askUnliftIO =
    ReaderT \r ->
    $ withUnliftIO \u ->
        pure $ UnliftIO $ unliftIO u . flip runReaderT r

newtype NoLoggingT m a = NoLoggingT { runNoLoggingT :: m a }
instance MonadUnliftIO m => MonadUnliftIO (NoLoggingT m) where
  askUnliftIO =
    NoLoggingT
    $ withUnliftIO \u ->
        pure $ UnliftIO $ unliftIO u . runNoLoggingT

newtype BlogT = BlogT (NoLoggingT (ReaderT MySQLConnectInfo m) a)
instance UnliftIO (BlogT m) where
  askUnliftIO :: BlogT m (UnliftIO (BlogT m))
  askUnliftIO =
    BlogT
                 -- u :: UnliftIO m
    $ withUnliftIO \u ->
                       -- unliftIO u :: m ~> IO
        pure $ UnliftIO $ unliftIO u . ?
                                    -- ? :: BlogT m ~> m
    -- ?1 :: BlogT m ~> NoLoggerT (ReaderT MySQLConnectInfo m)
    -- ?2 :: NoLoggerT (ReaderT MySQLConnectInfo m) ~> ReaderT MySQLConnectInfo m
    -- ?3 :: ReaderT MySQLConnectInfo m ~> m

-}

instance MonadUnliftIO m => MonadUnliftIO (BlogT m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO =
    BlogT
    $ NoLoggingT
    $ ReaderT \r ->
        withUnliftIO \u ->
            pure $ UnliftIO $ unliftIO u . flip runReaderT r . runNoLoggingT . unBlogT

runBlogT :: MySQLConnectInfo -> BlogT m a -> m a
runBlogT connInfo (BlogT m) =
  runReaderT (runNoLoggingT m) connInfo
