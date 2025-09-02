
module Exame2324 where

import Data.List


substs :: Eq a => (a,a) -> [a] -> [a]
substs t l = map (sub t) l
    where sub :: Eq a => (a, a) -> a -> a
          sub x y = if fst x == y then snd x else y


progressao :: [Int] -> Maybe Int
progressao [] = Nothing
progressao [_] = Nothing
progressao (x:h:t) = progressaoAc (h - x, h) t
    where progressaoAc :: (Int, Int) -> [Int] -> Maybe Int
          progressaoAc (d, _) [] = Just d
          progressaoAc (d, prev) (h:t) | h - prev == d = progressaoAc (d, h) t
                                       | otherwise = Nothing

removeElems :: [Int] -> [a] -> [a]
removeElems indexs l = removeElemsAc 0 si l
    where si = sort indexs
          removeElemsAc :: Int -> [Int] -> [a] -> [a]
          removeElemsAc _ [] l = l
          removeElemsAc _ _ [] = []
          removeElemsAc i (hi:ti) (h:t) | i == hi = removeElemsAc (i+1) ti t
                                        | otherwise = h : removeElemsAc (i+1) (hi:ti) t


type TabAbrev = [(Palavra,Abreviatura)]
type Palavra = String
type Abreviatura = String

ex :: TabAbrev
ex = [("muito","mt"),("que","q"),("maior",">"),("que","k"),("muito","mto")]


associa :: TabAbrev -> [(Palavra, [Abreviatura])]
associa [] = []
associa ((p, a) : xs) = let (iguais, resto) = separa p xs
                             abrevs = a : [snd b | b <- iguais]
                        in (p, abrevs) : associa resto

-- separa todos os pares cuja palavra é igual a `p`
separa :: Palavra -> TabAbrev -> (TabAbrev, TabAbrev)
separa _ [] = ([], [])
separa p ((q, a):xs) | p == q = ((q, a) : iguais, resto)
                     | otherwise = (iguais, (q, a) : resto)
    where (iguais, resto) = separa p xs


func :: Int -> [Int] -> [(Int,Int)]
func _ [] = []
func x (h:t) | x + h > 10 = (x, x + h) : func x t
             | otherwise = func x t


data BTree a = Empty | Node a (BTree a) (BTree a) deriving Eq

arv :: BTree Int
arv = Node 20 (Node 3 (Node 9 (Node 7 Empty Empty)
                      (Node 15 Empty Empty)) Empty)
              (Node 4 Empty (Node 3 Empty Empty))

-- não é árvore binária de procura
isTrace :: Eq a => [a] -> BTree a -> Bool
isTrace [] Empty = False
isTrace [] _ = False
isTrace (h:t) (Node value left right) | h == value && left == Empty && right == Empty = True
                                      | h == value = isTrace t left || isTrace t right 
                                      | otherwise = False


-- podia fazer subArv /= Empty, mas seria necessário (Eq a) => na declaração das funções
listaValores :: BTree a -> [[a]]
listaValores Empty = []
listaValores tree = niveis [tree]
    where niveis :: [BTree a] -> [[a]]
          niveis [] = []
          niveis nivelAtual = let valores = [x | Node x _ _ <- nivelAtual]
                              in valores : niveis [subArv | Node _ esq dir <- nivelAtual, subArv <- [esq, dir], case subArv of Empty -> False; _ -> True]

