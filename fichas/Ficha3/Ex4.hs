
module Ex4 where

import Data.Maybe

type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String

data Data = D Dia Mes Ano  deriving Show

type TabDN = [(Nome,Data)]

--a)
procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura nome (h:t) | nome == fst h = Just (snd h)
                   | otherwise = procura nome t

--b)
idade :: Data -> Nome -> TabDN -> Maybe Int
idade date n l | isNothing aniversario = Nothing
               | otherwise = Just (calcIdade date (fromJust aniversario))
    where aniversario = procura n l

-- Função auxiliar que calcula a idade de uma pessoa
calcIdade :: Data -> Data -> Int
calcIdade (D d m a) (D d' m' a') | m' < m || (m' == m && d' <= d) = a - a'
                                 | otherwise = a - a' - 1

--c)
anterior :: Data -> Data -> Bool
anterior d d' = valor >= 0
    where valor = calcIdade d' d

--d)
ordena :: TabDN -> TabDN
ordena [] = []
ordena (h:t) = insereData h (ordena t)

-- Função que insere uma data numa lista já ordenada
insereData :: (Nome,Data) -> TabDN -> TabDN
insereData x [] = [x]
insereData (n,d) ((n',d'):t) | anterior d d' = (n,d) : (n',d') : t
                             | otherwise = (n',d') : insereData (n,d) t

--e) 
porIdade :: Data -> TabDN -> [(Nome,Int)] 
porIdade _ [] = []
porIdade d l = porIdade' d (ordena l) 

-- Função auxiliar de porIdade que calcula a idade de cada pessoa e coloca na lista
porIdade' :: Data -> TabDN -> [(Nome,Int)] 
porIdade' _ [] = []
porIdade' d ((n,d'):t) = porIdade' d t ++ [(n,calcIdade d d')]

