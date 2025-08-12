

module Exame1617 where


unlines :: [String] -> String
unlines = undefined

(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) = undefined


data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a

primeiro :: Seq a -> a
primeiro = undefined

semUltimo :: Seq a -> Seq a
semUltimo = undefined


data BTree a = Empty | Node a (BTree a) (BTree a)

prune :: Int -> BTree a -> BTree a
prune = undefined

semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo = undefined


type Tabuleiro = [String]

exemplo :: Tabuleiro
exemplo = ["..R.",
           "R...",
           "...R",
           ".R.."]

posicoes :: Tabuleiro -> [(Int,Int)]
posicoes = undefined

valido :: Tabuleiro -> Bool
valido = undefined

bemFormado :: Int -> Tabuleiro -> Bool
bemFormado = undefined

