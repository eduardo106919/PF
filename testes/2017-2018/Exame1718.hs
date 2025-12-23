

module Exame1718 where

import Data.List

index :: [a] -> Int -> a
index [] _ = error "invalid value"
index (h:t) n = if n == 0 then h else index t (n-1)


data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao pos [] = pos
posicao (x,y) (h:t) = case h of
                        Norte -> posicao (x,y + 1) t
                        Sul -> posicao (x,y - 1) t
                        Este -> posicao (x + 1,y) t
                        Oeste -> posicao (x - 1,y) t

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (h:t) = if f h then True else any' f t


type Mat a = [[a]]

triSup :: (Eq a, Num a) => Mat a -> Bool
triSup m = and [ all (==0) (take i row) | (i, row) <- zip [0..] m ]

movimenta :: IO (Int, Int)
movimenta = loop (0,0)
    where loop (x,y) = do
            c <- getChar
            case c of
                'N' -> loop (x, y+1)
                'S' -> loop (x, y-1)
                'E' -> loop (x+1, y)
                'O' -> loop (x-1, y)
                _  -> return (x,y)

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

ex :: Imagem
ex = Mover (5,5)
      (Juntar [Mover (0,1) (Quadrado 5),
              Quadrado 4,
              Mover (4,3) (Quadrado 2)])

vazia :: Imagem -> Bool
vazia (Quadrado _) = False
vazia (Mover _ i) = vazia i
vazia (Juntar l) = all vazia l

maior :: Imagem -> Maybe Int
maior = maiorAcc 0
    where maiorAcc :: Int -> Imagem -> Maybe Int
          maiorAcc l (Quadrado x) = if x > l then Just x else Just l
          maiorAcc l (Mover _ i) = maiorAcc l i
          maiorAcc l (Juntar )


instance Eq Imagem where
    (==) = equalImagem

equalImagem :: Imagem -> Imagem -> Bool
equalImagem = undefined


