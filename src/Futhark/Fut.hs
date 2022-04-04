{-# LANGUAGE RankNTypes, ExistentialQuantification, FlexibleInstances, UndecidableInstances, TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables #-}
module Futhark.Fut (FutT, Fut, FutIO, MonadFut(..), runFutIn, runFutWith, runFut, runFutTIn, runFutTWith, runFutT, unsafeFromFutIO, unsafeLiftFromIO) where
import Futhark.Context
import Futhark.Config
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class
import Control.Monad.Identity
import Control.Monad.IO.Class
import System.IO.Unsafe
import Data.Functor.Identity

type FutT m = StateT Context m
type Fut = FutT Identity
type FutIO = FutT IO

class MonadIO m => MonadFut m where
    liftFut :: FutIO a -> m a

instance MonadFut FutIO where
  liftFut = id

instance (MonadFut m) => MonadFut (StateT s m) where
  liftFut = lift . liftFut

instance MonadThrow FutIO where
  throwM e = lift $ throwM e

runFutTIn :: Monad m => Context -> FutT m a -> m a
runFutTIn context a = evalStateT a context

runFutTWith :: Monad m => [ContextOption] -> FutT m a -> m a
runFutTWith options a
    = unsafePerformIO
    $ getContext options >>= \c -> return $ runFutTIn c a
runFutT :: Monad m => FutT m a -> m a
runFutT = runFutTWith []

runFutIn :: Context -> Fut a -> a
runFutIn context a = runIdentity $ runFutTIn context $ a

runFutWith :: [ContextOption] -> Fut a -> a
runFutWith options a = runIdentity $ runFutTWith options a
runFut = runFutWith []

unsafeFromFutIO :: forall a . FutIO a -> Fut a
unsafeFromFutIO a = do
  context <- get
  let (x, context') = unsafePerformIO $ (runStateT a context :: IO (a, Context))
  put context'
  return x

unsafeLiftFromIO :: forall a m . (MonadIO m) => (Context -> IO a) -> FutT m a
unsafeLiftFromIO a = do
  context <- get
  x <- liftIO (a context :: IO a)
  put context
  return x
