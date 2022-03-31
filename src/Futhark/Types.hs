{-# LANGUAGE RankNTypes, ExistentialQuantification, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Futhark.Types where
import qualified Futhark.Raw as Raw
import Futhark.Wrap
import Futhark.TypeClasses
import qualified Foreign as F
import qualified Data.Massiv.Array as M
import qualified Control.Concurrent.MVar as MV
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types (CBool(..), CSize(..), CChar(..), CFile(..))
import Foreign.Ptr (Ptr)
import Control.DeepSeq (rwhnf)
data F32_2d = F32_2d (MV.MVar Int) (F.ForeignPtr Raw.Futark_f32_2d)
data F32_1d = F32_1d (MV.MVar Int) (F.ForeignPtr Raw.Futark_f32_1d)
instance FutharkObject F32_2d Raw.Futark_f32_2d where
  wrapFO = F32_2d
  freeFO = Raw.free_f32_2d
  fromFO (F32_2d rc fp) = (rc, fp)
instance FutharkArray F32_2d Raw.Futark_f32_2d M.Ix2 Float where
  shapeFA = to2d Raw.shape_f32_2d
  newFA = from2d Raw.new_f32_2d
  valuesFA = Raw.values_f32_2d
instance FutharkObject F32_1d Raw.Futark_f32_1d where
  wrapFO = F32_1d
  freeFO = Raw.free_f32_1d
  fromFO (F32_1d rc fp) = (rc, fp)
instance FutharkArray F32_1d Raw.Futark_f32_1d M.Ix1 Float where
  shapeFA = to1d Raw.shape_f32_1d
  newFA = from1d Raw.new_f32_1d
  valuesFA = Raw.values_f32_1d
