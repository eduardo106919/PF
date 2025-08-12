
module Exame2122 where 

import System.Random

data LTree a = Tip a | Fork (LTree a) (LTree a)   deriving Show

data FTree a b = Leaf a | No b (FTree a b) (FTree a b)    deriving Show

type Mat a = [[a]]

data SReais = AA Double Double | FF Double Double 
            | AF Double Double | FA Double Double 
            | Uniao SReais SReais  

l :: LTree Int 
l = Fork (Fork 
               (Fork (Tip 3) (Tip 4))
               (Tip 5))
         (Fork 
               (Tip 9) 
               (Fork (Tip 0) (Tip 2)))

m :: Mat Int
m = [[1,2,3],[0,4,5],[0,0,6]]

r :: SReais 
r = Uniao 
        (Uniao  
             (AA 4.2 5.5) 
             (AF 3.1 7.0)) 
        (FF (-12.3) 30.0)

--1 
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x 

--2 
intersect' :: Eq a => [a] -> [a] -> [a] 
intersect' [] _ = []
intersect' _ [] = []
intersect' (h:t) l | h `elem` l = h : intersect' t l 
                   | otherwise = intersect' t l 

--3 
conv :: LTree Int -> FTree Int Int 
conv (Tip x) = Leaf x 
conv (Fork l r) = No (soma l + soma r) (conv l) (conv r)

soma :: LTree Int -> Int 
soma (Tip x) = x 
soma (Fork l r) = soma l + soma r 

--4 
triSup :: (Num a,Eq a) => Mat a -> Bool 
triSup m = zeros == ideal 
    where zeros = map (takeWhile (==0)) m 
          ideal = [replicate n 0 | n <- [0..(length m - 1)]]

--5
--a)  
showReais :: SReais -> String 
showReais (AA x y) = "]" ++ show x ++ "," ++ show y ++ "[" 
showReais (FF x y) = "[" ++ show x ++ "," ++ show y ++ "]" 
showReais (AF x y) = "]" ++ show x ++ "," ++ show y ++ "]" 
showReais (FA x y) = "[" ++ show x ++ "," ++ show y ++ "[" 
showReais (Uniao l r) = "(" ++ showReais l ++ " U " ++ showReais r ++ ")" 

instance Show SReais where 
    show = showReais 

--b) 
tira :: Double -> SReais -> SReais 
tira n (Uniao l r) = Uniao (tira n l) (tira n r)
tira n c = ajusta n c 

ajusta :: Double -> SReais -> SReais 
ajusta n c@(AA x y) | n <= x || n >= y = c 
                    | otherwise = Uniao (AA x n) (AA n y)
ajusta n c@(FF x y) | n < x || n > y = c 
                    | n == x = AF x y
                    | n == y = FA x y
                    | otherwise = Uniao (FA x n) (AF n y)
ajusta n c@(AF x y) | n <= x || n > y = c 
                    | n == y = AA x y
                    | otherwise = Uniao (AA x n) (AF n y) 
ajusta n c@(FA x y) | n >= y || n < x = c 
                    | n == x = AA x y
                    | otherwise = Uniao (FA x n) (AA n y) 

--6
func :: Float -> [(Float,Float)] -> [Float]
func x l = map snd (filter ((>x) . fst) l)

func' :: Float -> [(Float,Float)] -> [Float]
func' _ [] = []
func' n ((x,y):t) | x > n = y : func' n t 
                  | otherwise = func' n t 

--7 
subseqSum :: [Int] -> Int -> Bool 
subseqSum [] _ = False 
subseqSum l n | sum l == n = True 
              | otherwise = subseqSum (init l) n || subseqSum (tail l) n 

--8 
jogo :: Int -> (Int,Int) -> IO ()
jogo n (a,b) = do 
    seq <- sequence (replicate n (randomRIO (a,b)))
    putStr "Insira um número: "              
    num <- getLine 
    let val = read num :: Int 
        resp = subseqSum seq val 
    putStrLn ("Lista gerada: " ++ show seq)
    putStrLn ("Propriedade verificada: " ++ show resp)

