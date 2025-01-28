
module Ex2 where

--a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = (2*x) : dobros xs 

--b)
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre letra (x:xs) | letra == x = 1 + numOcorre letra xs
                       | otherwise = numOcorre letra xs 

--c)
positivos :: [Int] -> Bool
positivos [] = True 
positivos (x:xs) | x >= 0 = positivos xs 
                 | otherwise = False

--d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) | x >= 0 = x : soPos xs 
             | otherwise = soPos xs 

--e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs) | x < 0 = x + somaNeg xs 
               | otherwise = somaNeg xs 

--f)
tresUlt :: [a] -> [a]
tresUlt l | length l <= 3 = l 
          | otherwise = tresUlt $ tail l

--g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos (x:xs) = snd x : segundos xs 

--h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros e (x:xs) | e == fst x = True 
                      | otherwise = nosPrimeiros e xs 

--i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):xs) = (a+f,b+s,c+t)
    where (f,s,t) = sumTriplos xs

