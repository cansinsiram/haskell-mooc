-- Welcome to the first exercise set of the Haskell Mooc! Edit this
-- file according to the instructions, and check your answers with
--
--   stack runhaskell Set1Test.hs
--
-- You can also play around with your answers in GHCi with
--
--   stack ghci Set1.hs
--
-- This set contains exercises on
--   * defining functions
--   * basic expressions
--   * pattern matching
--   * recursion

module Set1 where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: define variables one and two. They should have type Int and
-- values 1 and 2, respectively.

one :: Int
one = 1

two :: Int
two = 2

------------------------------------------------------------------------------
-- Ex 2: define the function double of type Integer->Integer. Double
-- should take one argument and return it multiplied by two.

double :: Integer -> Integer
double x = x * 2

------------------------------------------------------------------------------
-- Ex 3: define the function quadruple that uses the function double
-- from the previous exercise to return its argument multiplied by
-- four.

quadruple :: Integer -> Integer
quadruple x = double (double x)

------------------------------------------------------------------------------
-- Ex 4: define the function distance. It should take four arguments of
-- type Double: x1, y1, x2, and y2 and return the (euclidean) distance
-- between points (x1,y1) and (x2,y2).

distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

------------------------------------------------------------------------------
-- Ex 5: define the function eeny that returns "eeny" for even inputs
-- and "meeny" for odd inputs.

eeny :: Integer -> String
eeny x = if even x then "eeny" else "meeny"

------------------------------------------------------------------------------
-- Ex 6: here's the function checkPassword from the course material.
-- Modify it so that it accepts two passwords, "swordfish" and
-- "mellon".

checkPassword :: String -> String
checkPassword password =
  if password == "swordfish" || password == "mellon"
    then "You're in."
    else "ACCESS DENIED!"

------------------------------------------------------------------------------
-- Ex 7: A postal service prices packages the following way.
-- Packages that weigh up to 500 grams cost 250 credits.
-- Packages over 500 and up to 5000 grams cost 300 credit + 1 credit per gram.
-- Packages over 5000 grams cost a constant 6000 credits.

postagePrice :: Int -> Int
postagePrice weight
  | weight <= 500 = 250
  | weight <= 5000 = 300 + weight
  | otherwise = 6000

------------------------------------------------------------------------------
-- Ex 8: define a function isZero that returns True if it is given an
-- Integer that is 0, and False otherwise. Give isZero a type signature.

isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False

------------------------------------------------------------------------------
-- Ex 9: implement using recursion a function sumTo such that
--   sumTo n
-- computes the sum 1+2+...+n

sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo n = n + sumTo (n - 1)

------------------------------------------------------------------------------
-- Ex 10: power n k should compute n to the power k (i.e. n^k)
-- Use recursion.
-- There's no need to handle negative values of k.

power :: Integer -> Integer -> Integer
power _ 0 = 1
power n k = n * power n (k - 1)

------------------------------------------------------------------------------
-- Ex 11: ilog3 n should be the number of times you can divide given
-- number by three (rounding down) before you get 0.

ilog3 :: Integer -> Integer
ilog3 0 = 0
ilog3 n = 1 + ilog3 (n `div` 3)
