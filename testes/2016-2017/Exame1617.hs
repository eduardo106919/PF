

module Exame1617 where

import Data.List


unlines' :: [String] -> String
unlines' [] = ""
unlines' [s] = s
unlines' (h:t) = h ++ "\n" ++ unlines' t

removePrimOcs :: (Eq a) => [a] -> [a] -> [a]
removePrimOcs l [] = l
removePrimOcs [] _ = []
removePrimOcs l (h:t) = removePrimOcs (removePrim h l) t
    where removePrim :: (Eq a) => a -> [a] -> [a]
          removePrim _ [] = []
          removePrim x (h:t) | x == h = t
                             | otherwise = h : removePrim x t


data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a

primeiro :: Seq a -> a
primeiro Nil = error "Sequência vazia"
primeiro (Inicio x _) = x
primeiro (Fim s _) = primeiro s

semUltimo :: Seq a -> Seq a
semUltimo Nil = error "Sequência vazia"
semUltimo (Inicio _ Nil) = Nil
semUltimo (Inicio x s) = Inicio x (semUltimo s)
semUltimo (Fim s _) = s


data BTree a = Empty | Node a (BTree a) (BTree a)

prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune _ Empty = Empty
prune n (Node x l r) = Node x (prune (n-1) l) (prune (n-1) r)

semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node _ Empty r) = r
semMinimo (Node x l r) = Node x (semMinimo l) r


type Tabuleiro = [String]

exemplo :: Tabuleiro
exemplo = ["..R.",
           "R...",
           "...R",
           ".R.."]

posicoes :: Tabuleiro -> [(Int,Int)]
posicoes = posicoesAcc 0
    where posicoesAcc :: Int -> Tabuleiro -> [(Int, Int)]
          posicoesAcc _ [] = []
          posicoesAcc n (h:t) = map (\ x -> (x,n)) (elemIndices 'R' h) ++ posicoesAcc (n+1) t

-- verifica se os elementos de uma lista são todos diferentes
allDifferent :: Eq a => [a] -> Bool
allDifferent xs = length xs == length (nub xs)

valido :: Tabuleiro -> Bool
valido t = allDifferent f && allDifferent s && allDifferent g
    where a = posicoes t
          (f,s,g) = foldr (\ (m, n) (x,y,z) -> (m:x, n:y, (m+n):z)) ([], [], []) a

bemFormado :: Int -> Tabuleiro -> Bool
bemFormado n t = length t == n && all (\ l -> length l == n) t && all (\ (x,y) -> x == 1 && x+y == n) aux
    where aux = map (\ l -> (length $ elemIndices 'R' l, length $ elemIndices '.' l)) t
