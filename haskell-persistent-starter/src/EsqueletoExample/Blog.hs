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
, UndecidableInstances
#-}

module EsqueletoExample.Blog
( runBlogT
) where

import Prelude

import Control.Monad.Base (MonadBase (..))
import Control.Monad.Logger (MonadLogger, NoLoggingT (..))
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Database.Persist.MySQL (MySQLConnectInfo)

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

deriving instance (MonadBase b m) => MonadBase b (BlogT m)

instance MonadTransControl BlogT where
  type StT BlogT a = StT NoLoggingT (StT (ReaderT MySQLConnectInfo) a)
  liftWith f =
    BlogT $ liftWith $ \run ->
      liftWith $ \run' ->
        f (run' . run . unBlogT)

  restoreT = BlogT . restoreT . restoreT

instance MonadBaseControl b m => MonadBaseControl b (BlogT m) where
  type StM (BlogT m) a = ComposeSt BlogT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

runBlogT :: MySQLConnectInfo -> BlogT m a -> m a
runBlogT connInfo (BlogT m) =
  runReaderT (runNoLoggingT m) connInfo
