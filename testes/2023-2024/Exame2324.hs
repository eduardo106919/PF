
module Exame2324 where


substs :: Eq a => (a,a) -> [a] -> [a]
substs t l = map (sub t) l
    where sub :: Eq a => (a, a) -> a -> a
          sub x y = if fst x == y then snd x else y


progressao :: [Int] -> Maybe Int
progressao = undefined

removeElems :: [Int] -> [a] -> [a]
removeElems = undefined


type TabAbrev = [(Palavra,Abreviatura)]
type Palavra = String
type Abreviatura = String

ex :: TabAbrev
ex = [("muito","mt"),("que","q"),("maior",">"),("que","k"),("muito","mto")]


associa :: TabAbrev -> [(Palavra,[Abreviatura])]
associa = undefined



func :: Int -> [Int] -> [(Int,Int)]
func _ [] = []
func x (h:t) | x + h > 10 = (x, x + h) : func x t
             | otherwise = func x t

data BTree a = Empty | Node a (BTree a) (BTree a)

arv :: BTree Int
arv = Node 20 (Node 3 (Node 9 (Node 7 Empty Empty)
                      (Node 15 Empty Empty)) Empty)
              (Node 4 Empty (Node 3 Empty Empty))

isTrace :: Eq a => [a] -> BTree a -> Bool
isTrace = undefined

listaValores :: BTree a -> [[a]]
listaValores = undefined




