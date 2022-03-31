{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RankNTypes          #-}
module Main where

import qualified Data.Massiv.Array as A
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Lens
import Control.Monad.IO.Class

import Futhark
import Futhark.Types
import qualified Futhark.Entries as E

import IO
import Show
import Data.Int

type Futhark1d t = A.Array A.S A.Ix1 t
type Futhark2d t = A.Array A.S A.Ix2 t
type Futhark3d t = A.Array A.S A.Ix3 t

message :: MonadIO m => String -> m ()
message string = liftIO $ putStrLn string

test :: (Int64, Int64)
            -> FutT IO ()
test (h, w) =
  do  keys <- E.randomField 777 h w
      values <- E.randomField 1313 h w
      (futKeys, futValues) <- E.sortBag keys values
      (futKeys, futValues, futRowAver, futColAver) <- E.sortBagAndDoStuff keys values
      (keys'   :: Futhark2d Float) <- fromFuthark futKeys
      (values' :: Futhark2d Float) <- fromFuthark futValues
      writeImageMono floatToWord8 ("output/"++ "keys_" ++ show h ++ "_" ++ show w ++".png") (keys')
      writeImageMono floatToWord8 ("output/"++ "values_" ++ show h ++ "_" ++ show w ++".png") (values')

main :: IO ()
main = do
  runFutT $ test (49152, 128)
