
module Ex1 where

-- a)
perimetro :: Float -> Float
perimetro raio = 2 * pi * raio

--b)
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt(disX + disY)
    where disX = (x2-x1)^2 
          disY = (y2-y1)^2
--c)
primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

--d)
multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0

--e) 
truncaImpar :: [Int] -> [Int]
truncaImpar l | mod (length l) 2 /= 0 = tail l
              | otherwise = l

--f)
max2 :: Int -> Int -> Int
max2 x y = if x > y then x else y

--g)
max3 :: Int -> Int -> Int -> Int 
max3 n1 n2 n3 = max2 (max2 n1 n2) n3

