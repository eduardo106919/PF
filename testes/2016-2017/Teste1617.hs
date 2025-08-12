

module Teste1617 where


type MSet a = [(a,Int)]

multiSet :: MSet Char
multiSet = [('b',4),('a',2),('c',1)]


cardMSet :: MSet a -> Int
cardMSet l = foldr (\ h r -> snd h + r) 0 l

moda :: MSet a -> [a]
moda [] = []
moda set = modaAc (head set) set
    where modaAc :: (a, Int) -> MSet a -> [a]
          modaAc _ [] = []
          modaAc a@(_, card) (h:t) | card == snd h = fst h : modaAc a t
                                   | otherwise = []

converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((x, y):t) = replicate y x ++ converteMSet t
-- esta versão evita a concatenação de listas, que é pouco eficiente
converteMSet ((x, y):t) | y > 1 = x : converteMSet ((x, y-1):t)
                        | y == 1 = x : converteMSet t

addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies set x val = insereElem b (x, val + a)
    where (a, b) = removeElem x set

removeElem :: Eq a => a -> MSet a -> (Int, MSet a)
removeElem _ [] = (0, [])
removeElem x (h:t) | x == fst h = (snd h, t)
                   | otherwise = (val, h : l)
    where (val, l) = removeElem x t

insereElem :: MSet a -> (a, Int) -> MSet a
insereElem [] x = [x]
insereElem (h:t) x | snd h < snd x = x : h : t
                   | otherwise = h : insereElem t x


data SReais = AA Double Double | FF Double Double
            | AF Double Double | FA Double Double
            | Uniao SReais SReais

subconjunto :: SReais
subconjunto = Uniao (Uniao (AA 4.2 5.5) (AF 3.1 7.0)) (FF (-12.3) 30.0)

instance Show SReais where
    show = showSReais

showSReais :: SReais -> String
showSReais (AA x y) = "]" ++ (show x) ++ "," ++ (show y) ++ "["
showSReais (FF x y) = "[" ++ (show x) ++ "," ++ (show y) ++ "]"
showSReais (AF x y) = "]" ++ (show x) ++ "," ++ (show y) ++ "]"
showSReais (FA x y) = "[" ++ (show x) ++ "," ++ (show y) ++ "["
showSReais (Uniao l r) = "(" ++ showSReais l ++ " U " ++ showSReais r ++ ")"

pertence :: Double -> SReais -> Bool
pertence value (AA x y) = (value > x) && (value < y)
pertence value (FF x y) = (value >= x) && (value <= y)
pertence value (AF x y) = (value > x) && (value <= y)
pertence value (FA x y) = (value >= x) && (value < y)
pertence value (Uniao l r) = (pertence value l) || (pertence value r)

tira :: Double -> SReais -> SReais
tira value a@(AA x y) = if (value > x) && (value < y) 
                            then (Uniao (AA x value) (AA value y)) 
                            else a
tira value a@(FF x y) = if (value >= x) && (value <= y) 
                            then (Uniao (FA x value) (AF value y)) 
                            else a
tira value a@(AF x y) = if (value > x) && (value <= y) 
                            then (Uniao (AA x value) (AF value y)) 
                            else a
tira value a@(FA x y) = if (value >= x) && (value < y) 
                            then (Uniao (FA x value) (AA value y)) 
                            else a
tira value (Uniao l r) = (Uniao (tira value l) (tira value r))


data RTree a = R a [RTree a]

percorre :: [Int] -> RTree a -> Maybe [a]
percorre = undefined

procura :: Eq a => a -> RTree a -> Maybe [Int]
procura = undefined

