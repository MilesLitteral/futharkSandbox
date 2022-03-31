{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE FlexibleContexts    #-}

module Show

where

import Data.Massiv.Array ( Array
                         , S
                         , Ix1(..)
                         , Ix2(..)
                         , Ix3(..)
                         , Sz1(..)
                         , Sz2(..)
                         , Sz3(..)
                         , Source(..)
                         , pattern Sz1
                         , pattern Sz2
                         , pattern Sz3
                         , toLists2
                         , toLists3
                         )

import Data.List (intersperse)

padL :: Int -> a -> [a] -> [a]
padL s p l
    | len >= s = l
    | otherwise    = replicate (s - len) p ++ l
      where len = length l

vectorRepresentation :: Show a => [a] -> String
vectorRepresentation row = "[" ++ concat (intersperse " " $ map (\y -> padL 2 ' ' (show y)) row) ++ "]"

textRepresentation :: Show a => [a] -> String
textRepresentation row = foldl (\acc y -> acc ++ (padL 4 ' ' (show y)) ++ " ") "" row

label :: Int -> String -> String
label i row = padL 3 ' ' (show i) ++ ": " ++ row

labelRows :: [String] -> [String]
labelRows = zipWith label [0..]

showGrid2d :: ( Source r a
              , Show a
              )
           => Array r Ix2 a -> String
showGrid2d grid = unlines $ labelRows $ map (textRepresentation) $ toLists2 grid

showGrid3d :: ( Source r a
              , Show a
              )
           => Array r Ix3 a -> String
showGrid3d grid = unlines $ labelRows $ map (concatMap vectorRepresentation) $ toLists3 grid
