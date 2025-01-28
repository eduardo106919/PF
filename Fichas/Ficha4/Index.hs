
module Index where 

import Data.Char 
import Data.List 

--1. 
digitAlpha :: String -> (String,String)
digitAlpha [] = ("","")
digitAlpha (h:t) | isAlpha h = (h:letras,nums)
                 | isDigit h = (letras,h:nums) 
                 | otherwise = (letras,nums)
    where (letras,nums) = digitAlpha t

--2. 
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h < 0 = (1 + neg,zer,pos)
          | h == 0 = (neg,1 + zer,pos)
          | otherwise = (neg,zer,1 + pos)
    where (neg,zer,pos) = nzp t

--3.                              
divMod' :: Integral a => a -> a -> (a,a)
divMod' x y | x < y = (0,x) 
            | otherwise = (1 + d,r)
        where (d,r) = divMod' (x-y) y 

--4.                        
fromDigits' :: [Int] -> Int 
fromDigits' l = fromDigitsAc l 0
    where fromDigitsAc :: [Int] -> Int -> Int 
          fromDigitsAc [] ac = ac 
          fromDigitsAc (x:xs) ac = fromDigitsAc xs (x + 10 * ac) 

--5. 
maxSumInit' :: (Num a,Ord a) => [a] -> a 
maxSumInit' l = maxSumInitAc [sum m | m <- inits l] 0 
    where maxSumInitAc :: (Num a,Ord a) => [a] -> a -> a 
          maxSumInitAc [] ac = ac 
          maxSumInitAc l ac | ac < sum l = maxSumInitAc (init l) (sum l)
                            | otherwise = maxSumInitAc (init l) ac 

--6.                                       
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib' (n-2) 0 1
                          
fib' :: Int -> Int -> Int -> Int 
fib' 0 n _ = n 
fib' i n n1 = fib' (1-1) n1 (n + n1) 

--7. 
intToStr :: Integer -> String 
intToStr 0 = "zero"
intToStr n = intToStrAc n ""

intToStrAc :: Integer -> String -> String 
intToStrAc 0 ('-':ac) = ac 
intToStrAc n ac = intToStrAc nn (( case r of 
                  0 -> "-zero"
                  1 -> "-um"
                  2 -> "-dois"
                  3 -> "-tres"
                  4 -> "-quatro"
                  5 -> "-cinco"
                  6 -> "-seis"
                  7 -> "-sete"
                  8 -> "-oito"
                  9 -> "-nove" ) ++ ac ) 
            where (nn,r) = n `divMod` 10

--8. 
--a) 
a = [6,12,18] 

a' = [x | x <- [1..20] , mod x 6 == 0]

--b)
b = [6,12,18] 

--c) 
c = [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]

c' = [(x,30-x) | x <- [10,20]]
--d)
d = [1,1,4,4,9,9,16,16,25,25]

d' = [x^2 | x <- [1..5] , y <- [1..2]]

--9. 
--a) 
lA = [2^y | y <- [0..10]]

--b) 
lB = [(x,y) | x <- [1..5], y <- [5,4,3,2,1], x+y==6]

--c) 
lC = [[1..x] | x <- [1,2,3,4,5]]
  
--d) 
lD = [replicate x 1 | x <- [1..5]]

--e)
lE = [fact x | x <- [1..6]]

fact :: Int -> Int 
fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

