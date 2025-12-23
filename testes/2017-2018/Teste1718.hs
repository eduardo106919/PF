

module Teste1718 where


insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t) | h > x = x : h : t
               | otherwise = h : insert x t


catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = case h of
                    Just x -> x : catMaybes t
                    Nothing -> catMaybes t

data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)


instance (Show a) => Show (Exp a) where
    show = showExp


showExp :: Show a => Exp a -> String
showExp (Const x) = show x
showExp (Var str) = str
showExp (Mais e1 e2) = "(" ++ showExp e1 ++ " + " ++ showExp e2 ++ ")"
showExp (Mult e1 e2) = "(" ++ showExp e1 ++ " * " ++ showExp e2 ++ ")"


insertOn :: (Ord b) => (a -> b) -> a -> [a] -> [a]
insertOn _ x [] = [x]
insertOn f x (y:ys) | f x <= f y = x:y:ys
                    | otherwise = y : insertOn f x ys

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn f (h:t) = insertOn f h (sortOn f t)



amplitude :: [Int] -> Int
amplitude [] = 0
amplitude [x] = 0
amplitude (x:y:ys) = amplitudeAcc x y ys
    where amplitudeAcc :: Int -> Int -> [Int] -> Int
          amplitudeAcc mi ma [] = ma - mi
          amplitudeAcc mi ma (h:t) | h < mi = amplitudeAcc h ma t
                                   | h > ma = amplitudeAcc mi h t
                                   | otherwise = amplitudeAcc mi ma t

parte :: [Int] -> ([Int],[Int])
parte = undefined


data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

ex :: Imagem
ex = Mover (5,5)
      (Juntar [Mover (0,1) (Quadrado 5),
              Quadrado 4,
              Mover (4,3) (Quadrado 2)])

conta :: Imagem -> Int
conta (Quadrado _) = 1
conta (Mover _ i) = conta i
conta (Juntar l) = foldr (\ t acc -> conta t + acc) 0 l

apaga :: Imagem -> IO Imagem
apaga i = do
            o <- randomRIO(1, n)
            let n = conta i

