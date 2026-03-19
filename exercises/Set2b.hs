module Set2b where

import Mooc.Todo

-- Some imports you'll need. Don't add other imports :)
import Data.List

------------------------------------------------------------------------------
-- Ex 1: binomial coefficients

binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n - 1) k + binomial (n - 1) (k - 1)

------------------------------------------------------------------------------
-- Ex 2: odd factorial

oddFactorial :: Integer -> Integer
oddFactorial 0 = 1
oddFactorial 1 = 1
oddFactorial n
  | even n = oddFactorial (n - 1)
  | otherwise = n * oddFactorial (n - 2)

------------------------------------------------------------------------------
-- Ex 3: Euclidean Algorithm (subtraction version)

myGcd :: Integer -> Integer -> Integer
myGcd a 0 = a
myGcd 0 b = b
myGcd a b
  | a > b = myGcd (a - b) b
  | otherwise = myGcd a (b - a)

------------------------------------------------------------------------------
-- Ex 4: leftpad

leftpad :: String -> Int -> String
leftpad s n
  | length s >= n = s
  | otherwise = replicate (n - length s) ' ' ++ s

------------------------------------------------------------------------------
-- Ex 5: countdown

countdown :: Integer -> String
countdown n = "Ready! " ++ go n ++ "Liftoff!"
  where
    go 0 = ""
    go k = show k ++ "... " ++ go (k - 1)

------------------------------------------------------------------------------
-- Ex 6: smallestDivisor

smallestDivisor :: Integer -> Integer
smallestDivisor n = helper 2
  where
    helper k
      | n `mod` k == 0 = k
      | otherwise = helper (k + 1)

------------------------------------------------------------------------------
-- Ex 7: isPrime

isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | otherwise = smallestDivisor n == n

------------------------------------------------------------------------------
-- Ex 8: biggestPrimeAtMost

biggestPrimeAtMost :: Integer -> Integer
biggestPrimeAtMost n
  | isPrime n = n
  | otherwise = biggestPrimeAtMost (n - 1)
