
module Ex3 where

import Data.List

type Mat a = [[a]]

mat :: Mat Int
mat = [[1,2,3],
       [0,4,5],
       [0,6,0]]
--a) 
dimOk :: Mat a -> Bool
dimOk m = all (\ l -> length l == comp) m
    where comp = length (head m)

--b)
dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat mat = (length mat, length (head mat))

--c)                            
addMat :: Num a => Mat a -> Mat a -> Mat a 
addMat m m' = zipWith (zipWith (+)) m m' 

--d)                            
transpose' :: Mat a -> Mat a 
transpose' ([]:t) = []
transpose' m = map head m : transpose' (map (drop 1) m) 

--e)                                            
multMat :: Num a => Mat a -> Mat a -> Mat a 
multMat m m' = map (\ l -> map sum (map (zipWith (*) l) m't)) m 
    where m't = transpose m' 

-- f) 
zipWhat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c 
zipWhat f m m' = zipWith (zipWith f) m m'

-- g)                               
triSup :: (Eq a,Num a) => Mat a -> Bool 
triSup m = nub (concat (map (\ l -> take (index m l) l) m)) == [0]

-- Função que determina o indice de um elemento de uma lista
index :: Eq a => [a] -> a -> Int 
index (h:t) n | n == h = 0 
              | otherwise = 1 + index t n 

-- h)                               
rotateLeft :: Mat a -> Mat a 
rotateLeft m = transpose $ map reverse m 

