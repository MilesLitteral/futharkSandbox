{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module IO
  ( ImageSize(..)
  , ImageFloat(..)
  , ImageWord8(..)
  , ImageWord16(..)
  , imageSize
  , withWord8
  , withWord16
  , word8ToFloat
  , floatToWord8
  , word16ToFloat
  , floatToWord16
  , readImage
  , decodeImage
  , writeImage
  , writeImageBool
  , writeImageMono
  , imgRgbaToArr3d
  , arr3dToImgRgba
  , flattenImg
  , unflattenImg
  , fromMonoImg
  )
where

import qualified Data.ByteString as BS (ByteString)
import qualified Data.Massiv.Array as A
import Data.Massiv.Array ( S(..)
                         , B(..)
                         , Comp(..)
                         , Sz1(..)
                         , Sz2(..)
                         , Sz3(..)
                         , pattern Sz1
                         , pattern Sz2
                         , pattern Sz3
                         , Ix1(..)
                         , Ix2(..)
                         , Ix3(..)
                         , pattern Ix1
                         , pattern Ix2
                         , pattern Ix3
                         , Load(..)
                         , Manifest(..)
                         , Array(..)
                         , (!)
                         )
import qualified Data.Massiv.Array.IO as I
import Data.Massiv.Array.IO ( Elevator(..)
                            , Pixel(..)
                            , Alpha(..)
                            )
import qualified Graphics.ColorModel as CM
import Data.Word
import Data.Int
import Control.Monad.IO.Class
import Control.Monad.Catch
import Foreign.C.Types

type Futhark1d t = Array S Ix1 t
type Futhark2d t = Array S Ix2 t
type Futhark3d t = Array S Ix3 t

type Array1d t = Array B Ix1 t
type Array2d t = Array B Ix2 t
type Array3d t = Array B Ix3 t


wORD8mAX  = 255
wORD16mAX = 65535
cHANNELrED   = 0
cHANNELgREEN = 1
cHANNELbLUE  = 2
cHANNELaLPHA = 3
nUMcHANNELS = 4

type ImageSize = A.Sz2

type ImageFloat  = Array S Ix3 Float
type ImageWord8  = Array S Ix3 Word8
type ImageWord16 = Array S Ix3 Word16


withWord8 :: Word8 -> Word8
withWord8 = id

withWord16 :: Word16 -> Word16
withWord16 = id

word8ToFloat :: Word8 -> Float
word8ToFloat i = fromIntegral i / wORD8mAX

floatToWord8 :: Float -> Word8
floatToWord8 i = round $ i * wORD8mAX

word16ToFloat :: Word16 -> Float
word16ToFloat i = fromIntegral i / 65535

floatToWord16 :: Float -> Word16
floatToWord16 i = round $ i * 65535

imageSize :: Array S Ix3 e -> ImageSize
imageSize img =
  let (A.Sz3 h w _) = A.size img
  in  A.Sz2 h w

readImage :: forall m e f
          . ( MonadIO m
            , Elevator e
            , A.Storable e
            , A.Storable f
            )
          => (e -> f)
          -> String
          -> m (Futhark3d f)
readImage f fileName =
  do img <- liftIO $ I.readImage fileName
     let img3d :: Futhark3d e = imgRgbaToArr3d img
     return $ A.compute $ A.map f img3d

decodeImage :: forall m e f
          . ( MonadIO m
            , MonadThrow m
            , Elevator e
            , A.Storable e
            , A.Storable f
            )
          => (e -> f)
          -> BS.ByteString
          -> m (Futhark3d f)
decodeImage f bs =
  do img <- I.decodePNG I.PNG bs
     let img3d :: Futhark3d e = imgRgbaToArr3d img
     return $ A.compute $ A.map f img3d


writeImage :: forall m e f
           .  ( MonadIO m
              , Elevator e
              , A.Storable e
              , A.Storable f
              )
           => (f -> e)
           -> String
           -> Futhark3d f
           -> m ()
writeImage f fileName arr =
  do let img :: Array S Ix2 (Pixel (Alpha CM.RGB) e) = arr3dToImgRgba $ A.compute $ A.map f arr
     liftIO $ I.writeImage fileName img

arrMonoToImgRgba :: Array S Ix2 Word8
                 -> Array S Ix2 (Pixel (Alpha CM.RGB) Word8)
arrMonoToImgRgba array =
  let (Sz2 arrayHeight arrayWidth) = A.size array
  in makeArray Seq (Sz2 arrayHeight arrayWidth) $
       \ (Ix2 y x) ->
       let val = array ! (Ix2 y x)
           r = val
           g = val
           b = val
           a = 255
       in  CM.PixelRGBA r g b a

writeImageMono :: forall m f
               .  ( MonadIO m
                  , A.Storable f
                  )
               => (f -> Word8)
               -> String
               -> Futhark2d f
               -> m ()
writeImageMono f fileName arr =
  do let img :: Array S Ix2 (Pixel (Alpha CM.RGB) Word8) = arrMonoToImgRgba $ A.compute $ A.map f arr
     liftIO $ I.writeImage fileName img



cBoolToImgRgba :: Array S Ix2 CBool
               -> Array S Ix2 (Pixel (Alpha CM.RGB) Word8)
cBoolToImgRgba array =
  let (Sz2 arrayHeight arrayWidth) = A.size array
  in makeArray Seq (Sz2 arrayHeight arrayWidth) $
       \ (Ix2 y x) ->
       let val = array ! (Ix2 y x)
           colr = if val > 0 then 255 else 0
           r = colr
           g = colr
           b = colr
           a = 255
       in  CM.PixelRGBA r g b a

writeImageBool :: ( MonadIO m
                  )
               => String
               -> Futhark2d CBool
               -> m ()
writeImageBool fileName arr =
  do let img :: Array S Ix2 (Pixel (Alpha CM.RGB) Word8) = cBoolToImgRgba arr
     liftIO $ I.writeImage fileName img

imgRgbaToArr3d :: ( A.Storable e
                  )
               => Array S Ix2 (Pixel (Alpha CM.RGB) e)
               -> Array S Ix3 e
imgRgbaToArr3d img =
  let (Sz2 imgHeight imgWidth) = A.size img
  in  makeArray Seq (Sz3 imgHeight imgWidth nUMcHANNELS) $
      \ (Ix3 y x channel) ->
        let (CM.PixelRGBA r g b a) = img ! (Ix2 y x)
        in
        case channel of
          0 -> r
          1 -> g
          2 -> b
          3 -> a

arr3dToImgRgba :: ( A.Storable e
                  )
               => Array S Ix3 e
               -> Array S Ix2 (Pixel (Alpha CM.RGB) e)
arr3dToImgRgba array =
  let (Sz3 arrayHeight arrayWidth _) = A.size array
  in makeArray Seq (Sz2 arrayHeight arrayWidth) $
       \ (Ix2 y x) ->
       let r = array ! (Ix3 y x cHANNELrED  )
           g = array ! (Ix3 y x cHANNELgREEN)
           b = array ! (Ix3 y x cHANNELbLUE )
           a = array ! (Ix3 y x cHANNELaLPHA)
       in  CM.PixelRGBA r g b a

unflattenImg :: (A.Size r) => Sz2 -> Array r  Ix1 e -> Array r Ix3 e
unflattenImg (Sz2 height width) = A.resize' (Sz3 height width nUMcHANNELS)

flattenImg :: (A.Size r) => Array r Ix3 e -> Array r Ix1 e
flattenImg = A.flatten

fromMonoImg ::( A.Size r
              , Manifest r Float
              )
              => Array r Ix2 Float
              -> ImageFloat
fromMonoImg array =
  let (Sz2 arrayHeight arrayWidth) = A.size array
  in makeArray Seq (Sz3 arrayHeight arrayWidth 4) $
       \ (Ix3 y x i) ->
           let gray = array ! (Ix2 y x)
           in  if i == cHANNELaLPHA then 1 else gray
