{-# LANGUAGE RankNTypes, ExistentialQuantification, FlexibleInstances, UndecidableInstances, TypeFamilies, MultiParamTypeClasses #-}
module Futhark.Fut (FutT, Fut, FutIO, MonadFut(..), runFutIn, runFutWith, runFut, runFutTIn, runFutTWith, runFutT, mapFutT, map2FutT, pureFut, unsafeFromFutIO, unsafeLiftFromIO) where
import Futhark.Context
import Futhark.Config
import System.IO.Unsafe
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Catch


newtype FutT m a = FutT (Context -> m a)

instance MonadTrans FutT where
    lift a = FutT (\_ -> a)
    {-# INLINEABLE lift #-}

instance Functor m => Functor (FutT m) where
    fmap f (FutT a) = FutT (fmap f.a)
    {-# INLINEABLE fmap #-}

instance Applicative m => Applicative (FutT m) where
    pure a = FutT (\_ -> pure a)
    (<*>) (FutT a) (FutT b) = FutT (\c -> a c <*> b c)
    {-# INLINEABLE pure #-}
    {-# INLINEABLE (<*>) #-}

instance Monad m => Monad (FutT m) where
    (>>=) (FutT a) f = FutT (\c -> a c >>= (\(FutT b) -> b c) . f)
    {-# INLINEABLE (>>=) #-}

instance MonadIO m => MonadIO (FutT m) where
   liftIO = lift . liftIO
   {-# INLINEABLE liftIO #-}

instance (MonadBase b m) => MonadBase b (FutT m) where
    liftBase = liftBaseDefault
    {-# INLINEABLE liftBase #-}

instance MonadTransControl (FutT) where
    type StT (FutT) a = a
    liftWith a = FutT (\c -> a (\(FutT a') -> a' c))
    restoreT = lift
    {-# INLINEABLE liftWith #-}
    {-# INLINEABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (FutT m) where
    type StM (FutT m) a = ComposeSt (FutT) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
    {-# INLINEABLE liftBaseWith #-}
    {-# INLINEABLE restoreM #-}

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

mapFutT :: (m a -> n b) -> FutT m a -> FutT n b
mapFutT f (FutT a) = FutT (f.a)
map2FutT :: (m a -> n b -> k c) -> FutT m a -> FutT n b -> FutT k c
map2FutT f (FutT a) (FutT b) = FutT (\c -> f (a c) (b c))


runFutTIn :: Context -> (forall c. FutT m a) -> m a
runFutTIn context (FutT a) = a context

runFutTWith :: [ContextOption] -> (forall c. FutT m a) -> m a
runFutTWith options a
    = unsafePerformIO
    $ getContext options >>= \c -> return $ runFutTIn c a
runFutT = runFutTWith []

runFutIn :: Context -> Fut a -> a
runFutIn context a = runIdentity $ runFutTIn context $ a

runFutWith :: [ContextOption] -> Fut a -> a
runFutWith options a = runIdentity $ runFutTWith options a
runFut = runFutWith []

pureFut :: (Monad m) => Fut a -> FutT m a
pureFut (FutT a) = FutT (pure . runIdentity . a)

unsafeFromFutIO :: FutIO a -> Fut a
unsafeFromFutIO (FutT a) = FutT (Identity . unsafePerformIO . a)

unsafeLiftFromIO :: Monad m => (Context -> IO a) -> FutT m a
unsafeLiftFromIO a = FutT (pure . unsafePerformIO . a)

