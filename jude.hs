module Jude where

import Data.List

--1.a

null' x
  | x == [] = True
  | otherwise = False


fst' (x,y) = x


snd' (x,y) = y


min' x y
  | x < y = x
  | otherwise = y


max' x y
  | x > y = x
  |otherwise = y
