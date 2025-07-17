
module Ex3 where
    
import Data.Char

{-
    ord -> recebe um caracter e dá um inteiro
    chr -> recebe um inteiro e dá um caracter
-}


--a)
soDigitos :: String -> String 
soDigitos [] = []
soDigitos (x:xs) | isDigit x = x : soDigitos xs 
                 | otherwise = soDigitos xs 

--b)
minusculas :: String -> Int 
minusculas [] = 0
minusculas (x:xs) | isLower x = 1 + minusculas xs 
                  | otherwise = minusculas xs 

--c)
nums :: String -> [Int] 
nums [] = []
nums (x:xs) | isDigit x = digitToInt x : nums xs 
            | otherwise = nums xs 

