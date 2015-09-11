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

filter' _ [] = []
filter' a [x]
  | a x == False = []
filter' a (x:xs)
  | a x == True = x:(filter' a xs)
  | otherwise = filter' a xs

--pembatas

delete' _ [] = []
delete' 0 (x:xs) = x:xs
delete' a (x:xs)
  | a == x = xs
  | otherwise = (x:delete' a xs)

--pembatas

deleteAll' _ [] = []
deleteAll' a (x:xs)
  | a == x = deleteAll' a xs
  | otherwise = deleteAll' a xs


--pembatas

foldl' x = x

--pembatas

foldl1' x = x

--pembatas

zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):(zip (xs) (ys))

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

intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' a (x:xs) = [x,a]++(intersperse' a xs)

--pembatas

intercalate' x = x

--pembatas

and' [] = True
and' (x:xs)
  | x == False = False
  | otherwise = and' xs

--pembatas

or' [] = False
or' (x:xs)
  | x == True = True
  | otherwise = or' xs

--pembatas

zip3' _ _ [] = []
zip3' _ [] _ = []
zip3' [] _ _ = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z):(zip3' (xs) (ys) (zs))

--pembatas

sum' [] = 0
sum' (x:xs) = x + (sum xs)


--pembatas

product' [] = 1
product' (x:xs) = x * product' xs


--pembatas

words' x = x

--pembatas

lines' h = [h]

--pembatas

unlines' x = x

--pembatas

unwords' x = x

--pembatas

takeWhile' _ [] = []
takeWhile' a (x:xs)
  | a x == False = []
  | a x == True = x:(takeWhile' a xs)


--pembatas

dropWhile' _ [] = []
dropWhile' a (x:xs)
  | a x == True = dropWhile' a xs
  | otherwise = x:xs

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

minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

--pembatas

maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)


--pembatas

inits' x = x

--pembatas

tails' [] = [[]]
tails' [x] = [[x]] ++ [[]]
tails' (x:xs) = (x:xs):(tails' xs)


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

replicate' 0 _ = []
replicate' a x = x:replicate' (a-1) x






--pembatas
-- First Assignment
-- Reimplement Haskell function
-- DON'T USE GOOGLE
