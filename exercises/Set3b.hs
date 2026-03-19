{-# LANGUAGE NoImplicitPrelude #-}

module Set3b where

import Mooc.LimitedPrelude
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1

buildList :: Int -> Int -> Int -> [Int]
buildList start count end
  | count <= 0 = [end]
  | otherwise = start : buildList start (count - 1) end

------------------------------------------------------------------------------
-- Ex 2

sums :: Int -> [Int]
sums i = helper 1 0
  where
    helper :: Int -> Int -> [Int]
    helper n acc
      | n > i = []
      | otherwise =
          let acc2 = acc + n
          in acc2 : helper (n + 1) acc2

------------------------------------------------------------------------------
-- Ex 3

mylast :: a -> [a] -> a
mylast def [] = def
mylast _ [x] = x
mylast def (_:xs) = mylast def xs

------------------------------------------------------------------------------
-- Ex 4

indexDefault :: [a] -> Int -> a -> a
indexDefault _ i def | i < 0 = def
indexDefault [] _ def = def
indexDefault (x:_) 0 _ = x
indexDefault (_:xs) i def = indexDefault xs (i - 1) def

------------------------------------------------------------------------------
-- Ex 5

sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:xs)
  | x <= y = sorted (y:xs)
  | otherwise = False

------------------------------------------------------------------------------
-- Ex 6

sumsOf :: [Int] -> [Int]
sumsOf xs = helper 0 xs
  where
    helper :: Int -> [Int] -> [Int]
    helper _ [] = []
    helper acc (y:ys) =
      let acc2 = acc + y
      in acc2 : helper acc2 ys

------------------------------------------------------------------------------
-- Ex 7

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

------------------------------------------------------------------------------
-- Ex 8

mymaximum :: (a -> a -> Bool) -> a -> [a] -> a
mymaximum _ initial [] = initial
mymaximum bigger initial (x:xs)
  | bigger x initial = mymaximum bigger x xs
  | otherwise = mymaximum bigger initial xs

------------------------------------------------------------------------------
-- Ex 9

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (a:as) (b:bs) = f a b : map2 f as bs

------------------------------------------------------------------------------
-- Ex 10

maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap _ [] = []
maybeMap f (x:xs) =
  case f x of
    Just y -> y : maybeMap f xs
    Nothing -> maybeMap f xs
