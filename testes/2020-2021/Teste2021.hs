
module Teste2021 where 

import Data.List
import Data.Maybe

type MSet a = [(a,Int)]

data BTree a = Empty | Node a (BTree a) (BTree a)

data FileSystem = File Nome | Dir Nome [FileSystem]
type Nome = String


a :: BTree Int 
a = Node 5 (Node 3 Empty Empty)
           (Node 7 Empty (Node 9 Empty Empty))

f :: FileSystem 
f = Dir "usr" [Dir "xxx" [File "abc.txt", 
                          File "readme", 
                          Dir "PF" [File "exemplo.hs"]
                         ],
               Dir "yyy" [], 
               Dir "zzz" [Dir "tmp" [], 
                          File "teste.c"
                         ]
              ]

--1 
elimina :: Eq a => [a] -> [a] -> [a]
elimina l [] = l
elimina l (h:t) = elimina (delete h l) t 

--2
--a) 
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
removeMSet _ [] = []
removeMSet x ((a,n):t) | x == a = if n <= 1 then t else (a,n-1) : t 
                       | otherwise = (a,n) : removeMSet x t 

--b) 
calcula :: MSet a -> ([a],Int) 
calcula = foldr (\ (x,y) (l,n) -> (x:l,y+n)) ([],0)
                                  
--3 
partes :: String -> Char -> [String]
partes l c = words p 
    where p = substitui l c 

-- Função que substitui o caracter indicado por ' ' 
substitui :: String -> Char -> String 
substitui [] _ = []
substitui (h:t) c | h == c = ' ' : substitui t c 
                  | otherwise = h : substitui t c 

--4
--a) 
remove :: Ord a => a -> BTree a -> BTree a  
remove _ Empty = Empty 
remove x (Node a l r) 
    | x < a = Node a (remove x l) r 
    | x > a = Node a l (remove x r) 
    | otherwise = case (l,r) of 
        (Empty, r) -> r 
        (l, Empty) -> l 
        (l,r) -> Node m l sm 
            where (m,sm) = minSmin r 

-- Função que indica o menor elemento da árvore e apresenta a árvore sem o menor elemento
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty r) = (x,r)
minSmin (Node x l r) = (m,Node x sm r) 
    where (m,sm) = minSmin l 

--b) 
showBT :: Show a => BTree a -> String 
showBT Empty = "*"
showBT (Node x l r) = "(" ++ showBT l ++ " <- " ++ show x ++ " -> " ++ showBT r ++ ")" 

instance (Show a) => Show (BTree a) where 
    show = showBT 

--5 
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' _ [] = []
sortOn' f (h:t) = insertOn f h (sortOn' f t)

-- Função que insere um elemento numa lista ordenada
insertOn :: Ord b => (a -> b) -> a -> [a] -> [a]
insertOn _ x [] = [x]
insertOn f x (h:t) | f x <= f h = x : h : t
                   | otherwise = h : insertOn f x t
 
--6
--a) 
fichs :: FileSystem -> [Nome]
fichs (File n) = [n]
fichs (Dir _ l) = concat $ map fichs l 

--b)     definição pouco eficiente, efetua trabalho repetitivo 
dirFiles :: FileSystem -> [Nome] -> Maybe [Nome]
dirFiles (File x) [] = Just [x]
dirFiles (Dir s l) [x] | s == x = Just (coleta l) 
                       | otherwise = Nothing 
dirFiles (Dir s l) (h:t) | s == h = case (procura l (head t)) of 
                                Just f -> dirFiles f t
                                _ -> Nothing 
                         | otherwise = Nothing 

-- Função que procura a diretoria indicada
procura :: [FileSystem] -> Nome -> Maybe FileSystem 
procura [] _ = Nothing 
procura (h:t) n = case h of 
    Dir s l -> if n == s then Just (Dir s l) else procura t n
    _ -> procura t n 

-- Função que recolhe os nomes dos ficheiros de uma diretoria
coleta :: [FileSystem] -> [Nome]
coleta [] = []
coleta (h:t) = case h of 
    File s -> s : coleta t 
    _ -> coleta t 

--c) 
listaFich :: FileSystem -> IO ()
listaFich f = do 
    putStr "Introduza uma path: " 
    p <- getLine 
    let path = partes p '/' 
        files = dirFiles f path 
    if files == Nothing 
    then putStr "Não é uma diretoria."
    else putStrLn (unwords (fromJust files))

