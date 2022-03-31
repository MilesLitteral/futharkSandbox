{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
module Futhark.Wrap where
import qualified Futhark.Raw as Raw
import Futhark.Context
import Futhark.Fut
import Futhark.TypeClasses
import Foreign as F
import qualified Foreign.Concurrent as FC
import Foreign.C
import qualified Data.Massiv.Array as M
import qualified Data.Massiv.Array.Unsafe as MU
import Control.Concurrent


wrapIn :: FutharkObject wrapped raw => Context -> Ptr raw -> IO wrapped
wrapIn context@(Context childCount pointer) rawObject = do
    modifyMVar_ childCount (\cc -> return $! (cc+1))
    referenceCounter <- newMVar 0
    pointer <- FC.newForeignPtr rawObject freeCall
    pure $! wrapFO referenceCounter pointer
    where freeCall = (inContextWithError context $ \c -> freeFO c rawObject)
                  >> modifyMVar_ childCount (\cc -> return $! (cc-1))

peekFree p = peek p >>= \v -> free p >> return v
peekFreeWrapIn context rawP
    = peek rawP >>= wrapIn context >>= \fo -> F.free rawP >> return fo

-- Ptr - Dim conversion

to1d f cP aP
       = f cP aP
       >>= fmap (\[d0] -> M.Sz1 d0)
       . fmap (fmap fromIntegral)
       . peekArray 1

to2d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1] -> M.Sz2 d0 d1)
       . fmap (fmap fromIntegral)
       . peekArray 2

to3d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1, d2] -> M.Sz3 d0 d1 d2)
       . fmap (fmap fromIntegral)
       . peekArray 3

to4d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1, d2, d3] -> M.Sz4 d0 d1 d2 d3)
       . fmap (fmap fromIntegral)
       . peekArray 4

to5d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1, d2, d3, d4] -> M.Sz5 d0 d1 d2 d3 d4)
       . fmap (fmap fromIntegral)
       . peekArray 5


from1d f cP eP (M.Sz1 d0)             = f cP eP (fromIntegral d0)

from2d f cP eP (M.Sz2 d0 d1)          = f cP eP (fromIntegral d0)
                                                (fromIntegral d1)

from3d f cP eP (M.Sz3 d0 d1 d2)       = f cP eP (fromIntegral d0)
                                                (fromIntegral d1)
                                                (fromIntegral d2)

from4d f cP eP (M.Sz4 d0 d1 d2 d3)    = f cP eP (fromIntegral d0)
                                                (fromIntegral d1)
                                                (fromIntegral d2)
                                                (fromIntegral d3)

from5d f cP eP (M.Sz5 d0 d1 d2 d3 d4) = f cP eP (fromIntegral d0)
                                                (fromIntegral d1)
                                                (fromIntegral d2)
                                                (fromIntegral d3)
                                                (fromIntegral d4)

