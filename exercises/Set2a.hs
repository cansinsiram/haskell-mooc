-- Exercise set 2:
--  * Guards
--  * Lists
--  * Maybe
--  * Either
--
-- Functions you will need:
--  * head, tail
--  * take, drop
--  * length
--  * null

module Set2a where

import Mooc.Todo

-- Some imports you'll need. Don't add other imports :)
import Data.List

------------------------------------------------------------------------------
-- Ex 1: Define the constant years, that is a list of the values 1982,
-- 2004 and 2020 in this order.

years = [1982, 2004, 2020]

------------------------------------------------------------------------------
-- Ex 2: define the function takeFinal, which returns the n last
-- elements of the given list.
--
-- If the list is shorter than n, return all elements.

takeFinal :: Int -> [a] -> [a]
takeFinal n xs = drop (length xs - n) xs

------------------------------------------------------------------------------
-- Ex 3: Update an element at a certain index in a list. More
-- precisely, return a list that is identical to the given list except
-- the element at index i is x.

updateAt :: Int -> a -> [a] -> [a]
updateAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

------------------------------------------------------------------------------
-- Ex 4: substring i j s should return the substring of s starting at
-- index i and ending at (right before) index j.

substring :: Int -> Int -> String -> String
substring i j s = take (j - i) (drop i s)

------------------------------------------------------------------------------
-- Ex 5: check if a string is a palindrome. A palindrome is a string
-- that is the same when read backwards.

isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

------------------------------------------------------------------------------
-- Ex 6: implement the function palindromify that chops a character
-- off the front _and_ back of a string until the result is a
-- palindrome.

palindromify :: String -> String
palindromify s
  | isPalindrome s = s
  | length s < 2 = ""
  | otherwise = palindromify (tail (init s))

------------------------------------------------------------------------------
-- Ex 7: implement safe integer division, that is, a function that
-- returns a Just result normally, but Nothing if the divisor is zero.

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv x y
  | y == 0 = Nothing
  | otherwise = Just (x `div` y)

------------------------------------------------------------------------------
-- Ex 8: implement a function greet that greets a person given a first
-- name and possibly a last name.

greet :: String -> Maybe String -> String
greet first Nothing = "Hello, " ++ first ++ "!"
greet first (Just last) = "Hello, " ++ first ++ " " ++ last ++ "!"

------------------------------------------------------------------------------
-- Ex 9: safe list indexing.

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 = Nothing
  | i >= length xs = Nothing
  | otherwise = Just (xs !! i)

------------------------------------------------------------------------------
-- Ex 10: another variant of safe division. This time you should use
-- Either to return a string error message.

eitherDiv :: Integer -> Integer -> Either String Integer
eitherDiv x y
  | y == 0 = Left (show x ++ "/" ++ show y)
  | otherwise = Right (x `div` y)

------------------------------------------------------------------------------
-- Ex 11: implement the function addEithers, which combines two values of type
-- Either String Int into one.

addEithers :: Either String Int -> Either String Int -> Either String Int
addEithers (Right x) (Right y) = Right (x + y)
addEithers (Left err) _ = Left err
addEithers _ (Left err) = Left err
