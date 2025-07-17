
module Teste2022_23 where 

type Mat = [[Int]] 

data Lista a = Esq a (Lista a) | Dir (Lista a) a | Nula

data BTree a = Empty | Node a (BTree a) (BTree a)    deriving Show

matriz :: String
matriz = "2,3,6,4\n12,3,12,4\n3,-4,5,7"

lista :: Lista Int
lista = Esq 1 (Dir (Dir (Esq 9 Nula) 3) 4)

arvore :: BTree Char 
arvore = Node 'a' (Node 'b' Empty Empty) (Node 'c' Empty Empty)

--1 
unlines' :: [String] -> String 
unlines' [] = []
unlines' [x] = x 
unlines' (h:t) = h ++ "\n" ++ unlines' t

--2 
stringToMat :: String -> Mat
stringToMat s = map stringToVector (lines s) 

stringToVector :: String -> [Int]
stringToVector s = map (\ n -> read n :: Int) sep 
    where sep = words $ switch s 

switch :: String -> String 
switch = map (\ c -> if c == ',' 
                       then ' ' 
                       else c)

--b) 
-- Função que efetua a transposta de uma matriz
transpose' :: Mat -> Mat 
transpose' ([]:t) = []
transpose' m = (map head m) : transpose' (map (drop 1) m)

-- Função que transfrorma uma lista de inteiros numa string
vetToStr :: [Int] -> String 
vetToStr [] = []
vetToStr [x] = show x 
vetToStr (h:t) = show h ++ "," ++ vetToStr t 

-- Função que transforma uma matriz de inteiros numa string
matToStr :: Mat -> String 
matToStr m = unlines (map vetToStr m)

transposta :: String -> String 
transposta s = matToStr trans 
    where mat = stringToMat s 
          trans = transpose' mat 

--3 
--a) 
semUltimo :: Lista a -> Lista a 
semUltimo Nula = Nula 
semUltimo (Esq x l) = Esq x (semUltimo l) 
semUltimo (Dir r _) = r 

--b) 
showLista :: Show a => Lista a -> [a]
showLista Nula = []
showLista (Esq x l) = x : showLista l 
showLista (Dir r x) = showLista r ++ [x]

instance (Show a) => Show (Lista a) where 
    show = show . showLista 

--4
inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node r e d) = (inorder e) ++ (r : inorder d)

--a) 
numeraAux :: Int -> BTree a -> (Int,BTree (a,Int)) 
numeraAux _ Empty = (0,Empty)
numeraAux n (Node x l r) = (1 + nl + nr, Node (x,n) ll rr) 
    where (nl,ll) = numeraAux (n-1) l 
          (nr,rr) = numeraAux (n+1) r 

numera :: BTree a -> BTree (a,Int)
numera b = arv 
    where (_,arv) = numeraAux 2 b

--b)                            TODO 
unInorder :: [a] -> [BTree a] 
unInorder = undefined 

