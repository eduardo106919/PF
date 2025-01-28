
module Ex8 where

import Data.Char 

--a)
isLower' :: Char -> Bool 
isLower' n = ord n >= ord 'a' && ord n <= ord 'z'

--b)
isDigit' :: Char -> Bool
isDigit' n = ord n >= ord '0' && ord n <= ord '9'

--c)
isAlpha' :: Char -> Bool 
isAlpha' c = ord c <= ord 'Z' && ord c >= ord 'A' || isLower c

--d)
toUpper' :: Char -> Char
toUpper' c = chr (ord c - 32)

--e)
intToDigit' :: Int -> Char
intToDigit' n = chr (n + 48)

--f)
digitToInt' :: Char -> Int
digitToInt' n = ord n - 48

