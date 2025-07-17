module Exame2019_20 where 

import System.Random

data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving Show

type Polinomio = [Coeficiente]
type Coeficiente = Float 

type Mat a = [[a]]

--1 
--a) 
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

--b) 
isPrefixOf :: Eq a => [a] -> [a] -> Bool 
isPrefixOf [] _ = True 
isPrefixOf _ [] = False 
isPrefixOf (h:t) (x:xs) = h == x && isPrefixOf t xs  

--2 
--a) 
folhas :: BTree a -> Int 
folhas Empty = 0 
folhas (Node _ Empty Empty) = 1 
folhas (Node _ l r) = folhas l + folhas r 

--b) 
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node x _ _) = [x]
path (h:t) (Node x l r) | h = x : path t r 
                        | otherwise = x : path t l 

--3
--a) 
valor :: Polinomio -> Float -> Float 
valor p v = foldr (\ (c,g) ac -> ac + c * v ^ g) 0 (zip p [0..])

--b)
deriv :: Polinomio -> Polinomio 
deriv p = map (\ (c,g) -> c*g) (zip [1..] (tail p))

--c) 
soma :: Polinomio -> Polinomio -> Polinomio
soma p p' = zipWith (+) p1 p2 
    where dif = abs (length p - length p')
          (p1,p2) = (p ++ aux, p' ++ aux)
          aux = replicate dif 0

--4 
--a) 
quebraLinha :: [Int] -> [a] -> [[a]]
quebraLinha [] _ = []
quebraLinha _ [] = []
quebraLinha (h:t) l = take h l : quebraLinha t (drop h l)

--b) 
fragmenta :: [Int] -> [Int] -> Mat a -> [Mat a]
fragmenta [] _ _ = []
fragmenta (h:t) l m = fragmenta' l (take h m) ++ fragmenta t l (drop h m)

-- Função que separa cada lista da matriz num certo indice
fragmenta' :: [Int] -> Mat a -> [Mat a]
fragmenta' [] _ = []
fragmenta' (h:t) m = map (take h) m : fragmenta' t (map (drop h) m)

--c) 
geraMat :: (Int,Int) -> (Int,Int) -> IO (Mat Int)
geraMat (x,y) (a,b) = do 
    let l = sequence $ replicate y $ randomRIO (a,b)
    sequence $ replicate x l 

