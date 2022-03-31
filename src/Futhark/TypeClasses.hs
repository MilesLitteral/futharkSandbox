{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances #-}
module Futhark.TypeClasses (FutharkObject, FutharkArray, freeFO, fromFO, withFO, wrapFO, addReferenceFO, finalizeFO, newFA, shapeFA, valuesFA, Input, Output, HasShape(..), fromFuthark, toFuthark) where
import qualified Futhark.Raw as Raw
import Futhark.Fut
import Foreign
import qualified Data.Massiv.Array as M
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Concurrent

class FutharkObject wrapped raw | wrapped -> raw, raw -> wrapped where
    wrapFO :: MVar Int -> ForeignPtr raw -> wrapped
    freeFO :: Ptr Raw.Futhark_context -> Ptr raw -> IO Int
    fromFO :: wrapped -> (MVar Int, ForeignPtr raw)

withFO :: FutharkObject wrapped raw => wrapped -> (Ptr raw -> IO b) -> IO b
withFO = withForeignPtr . snd . fromFO

addReferenceFO :: (MonadIO m, FutharkObject wrapped raw) => wrapped -> FutT m ()
addReferenceFO fo = liftIO $
    let (referenceCounter, _) = fromFO fo
     in modifyMVar_ referenceCounter (\r -> pure (r+1))

finalizeFO :: (MonadIO m, FutharkObject wrapped raw) => wrapped -> FutT m ()
finalizeFO fo = liftIO $
    let (referenceCounter, pointer) = fromFO fo
     in modifyMVar_ referenceCounter (\r
     -> if r > 0
            then pure (r-1)
            else finalizeForeignPtr pointer >> pure 0)


class (FutharkObject array rawArray, Storable element, M.Index dim)
    => FutharkArray array rawArray dim element
    | array -> dim, array -> element
    where
        shapeFA  :: Ptr Raw.Futhark_context -> Ptr rawArray -> IO (M.Sz dim)
        newFA    :: Ptr Raw.Futhark_context -> Ptr element -> M.Sz dim -> IO (Ptr rawArray)
        valuesFA :: Ptr Raw.Futhark_context -> Ptr rawArray -> Ptr element -> IO Int

class Input fo ho where
    toFuthark :: Monad m => ho -> FutT m fo

class Output fo ho where
    fromFuthark :: Monad m => fo -> FutT m ho

class HasShape fo dim where
    futharkShape :: Monad m => fo -> FutT m (M.Sz dim)
