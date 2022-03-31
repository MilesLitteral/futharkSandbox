
module Futhark.Entries where
import qualified Futhark.Raw as Raw
import qualified Futhark.Context as C
import Futhark.Fut (FutT)
import qualified Futhark.Fut as Fut
import qualified Futhark.Wrap as U
import Futhark.Types
import qualified Futhark.TypeClasses as T
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Foreign as F
import Foreign.C.Types
randomField :: Monad m => Int64 -> Int64 -> Int64 -> FutT m F32_2d
randomField seed b g
  = Fut.unsafeLiftFromIO
      $ (\ context
           -> do out0 <- F.malloc
                 C.inContextWithError
                   context
                   (\ context' -> Raw.entry_randomField context' out0 seed b g)
                 out0' <- U.peekFreeWrapIn context out0
                 return out0')
sortBag :: Monad m => F32_2d -> F32_2d -> FutT m (F32_2d, F32_2d)
sortBag keys values
  = Fut.unsafeLiftFromIO
      $ (\ context
           -> T.withFO keys
                $ (\ keys'
                     -> T.withFO values
                          $ (\ values'
                               -> do out0 <- F.malloc
                                     out1 <- F.malloc
                                     C.inContextWithError
                                       context
                                       (\ context'
                                          -> Raw.entry_sortBag context' out0 out1 keys' values')
                                     out0' <- U.peekFreeWrapIn context out0
                                     out1' <- U.peekFreeWrapIn context out1
                                     return (out0', out1'))))
sortBagAndDoStuff ::
  Monad m =>
  F32_2d -> F32_2d -> FutT m (F32_2d, F32_2d, F32_1d, F32_1d)
sortBagAndDoStuff keys values
  = Fut.unsafeLiftFromIO
      $ (\ context
           -> T.withFO keys
                $ (\ keys'
                     -> T.withFO values
                          $ (\ values'
                               -> do out0 <- F.malloc
                                     out1 <- F.malloc
                                     out2 <- F.malloc
                                     out3 <- F.malloc
                                     C.inContextWithError
                                       context
                                       (\ context'
                                          -> Raw.entry_sortBagAndDoStuff
                                               context' out0 out1 out2 out3 keys' values')
                                     out0' <- U.peekFreeWrapIn context out0
                                     out1' <- U.peekFreeWrapIn context out1
                                     out2' <- U.peekFreeWrapIn context out2
                                     out3' <- U.peekFreeWrapIn context out3
                                     return (out0', out1', out2', out3'))))
