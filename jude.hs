-- First Assignment
-- Reimplement Haskell function

module Jude where

import Data.List

-- 1.a

null' x
  | x == [] = True
  | otherwise = False

--pembatas

take' a [] = []
take' 0 (x:xs) = []
take' a (x:xs) = x:take' (a-1) xs

--pembatas

drop' a [] = []
drop' 0 (x:xs) = x:xs
drop' a (x:xs) = drop' (a - 1) xs

--pembatas

fst' (x,y) = x

--pembatas

snd' (x,y) = y

--pembatas

map' f [] = []
map' f (x:xs) = f x : map' f xs


--pembatas

filter' x = x

--pembatas

delete' _ [] = []
delete' 0 (x:xs) = x:xs
delete' a (x:xs)
  | a == x = xs
  | otherwise = (x:delete' a xs)

--pembatas

deleteAll' x = x

--pembatas

foldl' x = x

--pembatas

foldl1' x = x

--pembatas

zip' x = x

--pembatas

zipWith' x = x

--pembatas

nth' (x:xs) a
  | a == 0 = x
  | a > 0 = nth (xs) (a-1)

--pembatas

scanl' x = x

--pembatas

scanl1' x = x

--pembatas

elem' a [] = False
elem' a (x:xs)
  | x == a = True
  | a /= x = elem' a (xs)
  | otherwise = False


--pembatas

notElem' a [] = True
notElem' a (x:xs)
  | x == a = False
  | a /= x = notElem' a (xs)
  | otherwise = True


--pembatas

head' (x:xs) = x

--pembatas

length' [] = 0
length' (x:xs) = 1 + (length' xs)

--pembatas

reverse' [] = []
reverse' (x:xs) = (reverse' (xs)) ++ [x]


--pembatas

last' [x] = x
last' (x:xs) = x * 0 + last' xs

--pembatas

tail' (x:xs) = xs

--pembatas

init' [a] = []
init' (x:xs) = x:(init' xs)

--pembatas

max' x y
  | x > y = x
  |otherwise = y

--pembatas

min' x y
  | x < y = x
  | otherwise = y

--pembatas

concat' x = x

--pembatas

intersperse' x = x

--pembatas

intercalate' x = x

--pembatas

and' x = x

--pembatas

or' x = x

--pembatas

zip3' x = x

--pembatas

sum' [] = 0
sum' (x:xs) = x + (sum xs)


--pembatas

product' [] = 1
product' (x:xs) = x * product' xs


--pembatas

words' x = x

--pembatas

lines' z = ["z"]

--pembatas

unlines' x = x

--pembatas

unwords' x = x

--pembatas

takeWhile' x = x

--pembatas

dropWhile' x = x

--pembatas

concatMap' x = x

--pembatas

all' _ [] = True
all' a (x:xs)
  | a x == False = False
  | otherwise = all' a xs

--pembatas

any' _ [] = False
any' a (x:xs)
  | a x == True = True
  | otherwise = any' a xs

--pembatas

insert' a [] = [a]

insert' a (x:xs)
  | a <= x = (a:x:xs)
  | a > x = x:(insert' a xs)

--pembatas

zipWith3' x = x

--pembatas

-- 1.b

nub' x = x

--pembatas

sort' x = x

--pembatas

minimum' x = x

--pembatas

maximum' x = x

--pembatas

inits' x = x

--pembatas

tails' x = x

--pembatas

union' x = x

--pembatas

intersect' x = x

--pembatas

group' [] = [[]]
group' [x] = [[x]]
group' (x:xs) = [[x]] ++ group' xs

--pembatas

splitAt' x = x

--pembatas

partition' x = x

--pembatas

replicate' x = x

--pembatas
-- First Assignment
-- Reimplement Haskell function
-- DON'T USE GOOGLE
