
module Teste2324 where

import Data.List


alterna :: Num a => Int -> a -> [a]
alterna n v | n <= 0 = []
            | otherwise = v : alterna (n-1) (-v)


data Turma = Empty | Node (Integer, String) Turma Turma

instance Show Turma where
    show = showTurma

showTurma :: Turma -> String
showTurma Empty = ""
showTurma (Node (num, nome) left right) = showTurma left ++ "(" ++ (show num) ++ ", " ++ nome ++")\n" ++ showTurma right


limites :: Turma -> (Integer,Integer)
limites = undefined


type TabAbrev = [(Palavra,Abreviatura)]
type Palavra = String
type Abreviatura = String

difMaior :: TabAbrev -> (Palavra, Int)
difMaior [] = error "Tabela vazia"
difMaior tab = difMaiorAc ("", 0) tab
    where difMaiorAc :: (Palavra, Int) -> TabAbrev -> (Palavra, Int)
          difMaiorAc t [] = t
          difMaiorAc ac@(_, n) (h:t) | n < diff = difMaiorAc (fst h, diff) t
                                     | otherwise = difMaiorAc ac t
              where diff = length (fst h) - length (snd h)

subst :: [String] -> TabAbrev -> [String]
subst l tab = map (replaceWord tab) l
    where replaceWord :: TabAbrev -> String -> String
          replaceWord t str = case abv of
                                Nothing -> str
                                Just s -> s
              where abv = lookup str t


data LTree a = Tip a | Fork (LTree a) (LTree a)

dumpLT :: LTree a -> [(a,Int)]
dumpLT tree = dumpLTAc 1 tree
    where dumpLTAc :: Int -> LTree a -> [(a, Int)]
          dumpLTAc nivel (Tip x) = [(x, nivel)]
          dumpLTAc nivel (Fork l r) = dumpLTAc (nivel+1) l ++ dumpLTAc (nivel+1) r


unDumpLT :: [(a,Int)] -> LTree a
unDumpLT = undefined


