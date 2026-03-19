module Set3a where

import Mooc.Todo

-- Some imports you'll need.
-- Do not add any other imports! :)
import Data.Char
import Data.Either
import Data.List

------------------------------------------------------------------------------
-- Ex 1

maxBy :: (a -> Int) -> a -> a -> a
maxBy measure a b
  | measure a >= measure b = a
  | otherwise = b

------------------------------------------------------------------------------
-- Ex 2

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

------------------------------------------------------------------------------
-- Ex 3

mapMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
mapMaybe2 f (Just x) (Just y) = Just (f x y)
mapMaybe2 _ _ _ = Nothing

------------------------------------------------------------------------------
-- Ex 4

palindromeHalfs :: [String] -> [String]
palindromeHalfs xs = map firstHalf (filter palindrome xs)

firstHalf :: String -> String
firstHalf xs = take ((length xs + 1) `div` 2) xs

palindrome :: String -> Bool
palindrome xs = xs == reverse xs

------------------------------------------------------------------------------
-- Ex 5

capitalize :: String -> String
capitalize = unwords . map capitalizeFirst . words
  where
    capitalizeFirst :: String -> String
    capitalizeFirst [] = []
    capitalizeFirst (x:xs) = toUpper x : xs

------------------------------------------------------------------------------
-- Ex 6

powers :: Int -> Int -> [Int]
powers k max = takeWhile (<= max) (iterate (* k) 1)

------------------------------------------------------------------------------
-- Ex 7

while :: (a->Bool) -> (a->a) -> a -> a
while check update value
  | check value = while check update (update value)
  | otherwise = value

------------------------------------------------------------------------------
-- Ex 8

whileRight :: (a -> Either b a) -> a -> b
whileRight check x =
  case check x of
    Left y -> y
    Right z -> whileRight check z

-- for the whileRight examples:
-- step k x doubles x if it's less than k
step :: Int -> Int -> Either Int Int
step k x = if x<k then Right (2*x) else Left x

-- bomb x implements a countdown: it returns x-1 or "BOOM" if x was 0
bomb :: Int -> Either String Int
bomb 0 = Left "BOOM"
bomb x = Right (x-1)

------------------------------------------------------------------------------
-- Ex 9

joinToLength :: Int -> [String] -> [String]
joinToLength n xs = [a ++ b | a <- xs, b <- xs, length (a ++ b) == n]

------------------------------------------------------------------------------
-- Ex 10

(+|+) :: [a] -> [a] -> [a]
[] +|+ [] = []
(x:_) +|+ [] = [x]
[] +|+ (y:_) = [y]
(x:_) +|+ (y:_) = [x,y]

------------------------------------------------------------------------------
-- Ex 11

sumRights :: [Either a Int] -> Int
sumRights = sum . map (either (const 0) id)

------------------------------------------------------------------------------
-- Ex 12

multiCompose :: [a -> a] -> a -> a
multiCompose fs = foldr (.) id fs

------------------------------------------------------------------------------
-- Ex 13

multiApp :: ([b] -> c) -> [a -> b] -> a -> c
multiApp f gs = f . map ($)

------------------------------------------------------------------------------
-- Ex 14

interpreter :: [String] -> [String]
interpreter commands = go 0 0 commands
  where
    go :: Int -> Int -> [String] -> [String]
    go _ _ [] = []
    go x y ("up":cs) = go x (y+1) cs
    go x y ("down":cs) = go x (y-1) cs
    go x y ("left":cs) = go (x-1) y cs
    go x y ("right":cs) = go (x+1) y cs
    go x y ("printX":cs) = show x : go x y cs
    go x y ("printY":cs) = show y : go x y cs
    go x y (_:cs) = go x y cs
