
module Teste2021_22 where 

import Data.List
import System.Random (randomRIO)

type Mat a = [[a]]

type Nome = String 
type Telefone = Integer 
data Agenda = Vazia | Nodo (Nome,[Telefone]) Agenda Agenda

data RTree a = R a [RTree a]        deriving Show
type Dictionary = [RTree (Char,Maybe String)]

a :: Agenda 
a = Nodo ("edu",[123,456,789]) 
            (Nodo ("ana",[999,333]) 
                    Vazia 
                    (Nodo ("bernardo",[444]) Vazia Vazia))
            (Nodo ("jervasio",[222,555,777,888]) 
                    Vazia
                    (Nodo ("maria",[111]) Vazia Vazia) )

d :: Dictionary
d = [R ('c',Nothing) [
       R ('a',Nothing) [
           R ('r',Nothing) [
               R ('a',Just "...") [
                   R ('s',Just "...") [] ],
               R ('o',Just "...") [],
               R ('r',Nothing) [
                   R ('o',Just "...") [] ]
    ]  ]   ]   ]

--1 
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h:t) (a:as) = (h,a) : zip' t as 

--2 
preCrescente :: Ord a => [a] -> [a] 
preCrescente [] = []
preCrescente [a] = [a]
preCrescente (h:s:t) | h < s = h : preCrescente (s:t) 
                     | otherwise = [h]

--3 
amplitude :: [Int] -> Int 
amplitude [] = 0 
amplitude (h:t) = maior - menor 
    where (maior,menor) = aux t (h,h)

-- Função auxiliar de amplitude, que determina o maior e menor elemento da lista, com acumulador
aux :: [Int] -> (Int,Int) -> (Int,Int)
aux [] g = g 
aux (h:t) (x,y) | h > x = aux t (h,y) 
                | h < y = aux t (x,h) 
                | otherwise = aux t (x,y) 

--4 
soma :: Num a => Mat a -> Mat a -> Mat a 
soma = zipWith (zipWith (+)) 

--5 
showAgenda :: Agenda -> String 
showAgenda ag = concat $ map (\ (n,l) -> n ++ " -- " ++ mostra l ++ "\n") org 
    where org = coleta ag

coleta :: Agenda -> [(Nome,[Telefone])]
coleta Vazia = []
coleta (Nodo (n,l) e d) = coleta e ++ [(n,l)] ++ coleta d

mostra :: [Telefone] -> String 
mostra [] = []
mostra [x] = show x 
mostra (h:t) = show h ++ " / " ++ mostra t 

instance Show Agenda where 
    show = showAgenda 

--6 
randomSel :: (Eq a) => Int -> [a] -> IO [a]
randomSel n l = randomSel' n l []

randomSel' :: (Eq a) => Int -> [a] -> [a] -> IO [a]
randomSel' 0 _ acc = return acc
randomSel' n l acc = do 
    if n > length l 
    then return l 
    else do i <- randomRIO (0,length l - 1)
            let index = read (show i) :: Int 
                obj = l !! index 
            randomSel' (n-1) (delete obj l) (obj:acc) 

--7 
-- Função que remove os elementos repetidos 
organiza' :: Eq a => [(a,[Int])] -> [(a,[Int])] 
organiza' [] = []
organiza' (h:t) | h `elem` t = organiza' t 
                | otherwise = h : organiza' t

organiza :: Eq a => [a] -> [(a,[Int])]
organiza l = organiza' list 
    where list = map (\ x -> (x,elemIndices x l)) l

--8 
func :: [[Int]] -> [Int]
func l = concat (filter (\ x -> sum x > 10) l)

func' :: [[Int]] -> [Int]
func' [] = []
func' (h:t) | sum h > 10 = h ++ func' t 
            | otherwise = func t 

--9 
insere :: String -> String -> Dictionary -> Dictionary
insere [] _ d = d 
insere [x] i [] = [insere' [x] i]
insere [x] i ((R (c,f) l):ys) 
    | x < c = (R (x,Just i) []) : (R (c,f) l) : ys 
    | x == c = (R (c,Just i) l) : ys 
    | otherwise = if null ys 
                  then (R (c,f) l) : [R (x,Just i) []]
                  else (R (c,f) l) : insere [x] i ys 
insere (h:t) i ((R (c,f) l):r) 
    | h < c = insere' (h:t) i : (R (c,f) l) : r 
    | h == c = (R (c,f) (insere t i l)) : r 
    | otherwise = if null r
                  then (R (c,f) l) : [insere' (h:t) i] 
                  else (R (c,f) l) : insere (h:t) i r 

-- Funçaõ que cria uma RTree com a palavra e a informação indicada 
insere' :: String -> String -> RTree (Char,Maybe String)
insere' [x] i = R (x,Just i) []
insere' (h:t) i = R (h,Nothing) [insere' t i]

