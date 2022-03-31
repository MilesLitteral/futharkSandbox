{-# LANGUAGE RankNTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Futhark.Utils where
import qualified Futhark.Raw as Raw
import Futhark.Context
import Futhark.Fut
import Futhark.TypeClasses
import Futhark.Wrap
import Foreign as F
import qualified Foreign.Concurrent as FC
import Foreign.C
import qualified Data.Massiv.Array as M
import qualified Data.Massiv.Array.Unsafe as MU


instance (FutharkArray array rawArray dim element)
  => Input array (M.Array M.S dim element) where
    toFuthark array = unsafeLiftFromIO $ \context
      -> inContext context $ \c
      -> MU.unsafeWithPtr array (\aP -> newFA c aP $ M.size array)
      >>= wrapIn context

instance (FutharkArray array rawArray dim element)
  => Output array (M.Array M.S dim element) where
    fromFuthark array = unsafeLiftFromIO $ \context
      -> inContext context $ \c
      -> withFO array $ \aP
      -> do
          syncContext context
          shape <- shapeFA c aP
          pointer <- mallocForeignPtrArray $ M.totalElem shape
          withForeignPtr pointer $ valuesFA c aP
          return $ M.resize' shape
                 $ MU.unsafeArrayFromForeignPtr0 M.Seq pointer
                 $ M.Sz1 (M.totalElem shape)

instance (FutharkArray array rawArray dim element)
  => HasShape array dim where
    futharkShape array = unsafeLiftFromIO $ \context
      -> inContext context $ \c
      -> withFO array $ \aP
      -> do
          shape <- shapeFA c aP
          return shape

boolToCBool :: Bool -> CBool
boolToCBool True  = CBool 1
boolToCBool False = CBool 0

fromFutharkT2 (a, b) = do
    a' <- fromFuthark a
    b' <- fromFuthark b
    return (a', b')

fromFutharkT3 (a, b, c) = do
    a' <- fromFuthark a
    b' <- fromFuthark b
    c' <- fromFuthark c
    return (a', b', c')

fromFutharkT4 (a, b, c, d) = do
    a' <- fromFuthark a
    b' <- fromFuthark b
    c' <- fromFuthark c
    d' <- fromFuthark d
    return (a', b', c', d')

fromFutharkT5 (a, b, c, d, e) = do
    a' <- fromFuthark a
    b' <- fromFuthark b
    c' <- fromFuthark c
    d' <- fromFuthark d
    e' <- fromFuthark e
    return (a', b', c', d', e')

fromFutharkT6 (a, b, c, d, e, f) = do
    a' <- fromFuthark a
    b' <- fromFuthark b
    c' <- fromFuthark c
    d' <- fromFuthark d
    e' <- fromFuthark e
    f' <- fromFuthark f
    return (a', b', c', d', e', f')

fromFutharkT7 (a, b, c, d, e, f, g) = do
    a' <- fromFuthark a
    b' <- fromFuthark b
    c' <- fromFuthark c
    d' <- fromFuthark d
    e' <- fromFuthark e
    f' <- fromFuthark f
    g' <- fromFuthark g
    return (a', b', c', d', e', f', g')


toFutharkT2 (a, b) = do
    a' <- toFuthark a
    b' <- toFuthark b
    return (a', b')

toFutharkT3 (a, b, c) = do
    a' <- toFuthark a
    b' <- toFuthark b
    c' <- toFuthark c
    return (a', b', c')

toFutharkT4 (a, b, c, d) = do
    a' <- toFuthark a
    b' <- toFuthark b
    c' <- toFuthark c
    d' <- toFuthark d
    return (a', b', c', d')

toFutharkT5 (a, b, c, d, e) = do
    a' <- toFuthark a
    b' <- toFuthark b
    c' <- toFuthark c
    d' <- toFuthark d
    e' <- toFuthark e
    return (a', b', c', d', e')

toFutharkT6 (a, b, c, d, e, f) = do
    a' <- toFuthark a
    b' <- toFuthark b
    c' <- toFuthark c
    d' <- toFuthark d
    e' <- toFuthark e
    f' <- toFuthark f
    return (a', b', c', d', e', f')

toFutharkT7 (a, b, c, d, e, f, g) = do
    a' <- toFuthark a
    b' <- toFuthark b
    c' <- toFuthark c
    d' <- toFuthark d
    e' <- toFuthark e
    f' <- toFuthark f
    g' <- toFuthark g
    return (a', b', c', d', e', f', g')

