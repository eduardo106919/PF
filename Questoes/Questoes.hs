
module Questoes where 

data Movimento = Norte | Sul | Este | Oeste
        deriving Show

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

data Equipamento = Bom | Razoavel | Avariado
        deriving Show

--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x1 x2 | x1 > x2 = []
                  | otherwise = x1 : enumFromTo' (x1+1) x2

--2                                                
enumFromThenTo' :: Int -> Int -> Int -> [Int]          
enumFromThenTo' s n e | s > e && n >= s || s < e && n < s = []
                      | otherwise = s : enumFromThenTo' n (n + n-s) e

--3
juntar :: [a] -> [a] -> [a]
juntar [] l = l
juntar (x:xs) lista = x : juntar xs lista

--4                           
local :: [a] -> Int -> a  
local (h:t) val | val == 0 = h 
                | otherwise = local t (val-1)

--5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--6                           
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' val (h:t) = h : take' (val-1) t   

--7                            
drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' _ [] = []
drop' val (_:t) = drop' (val-1) t

--8                            
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h:t) (x:xs) = (h,x) : zip' t xs 

--9                             
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' val ele = ele : replicate' (val-1) ele 

--10                            
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' ele (h:t) = h : ele : intersperse' ele t

--11
group' :: Eq a => [a] -> [[a]] 
group' [] = []
group' (h:t) = aux : group' (drop l (h:t)) 
    where aux = h : groupAux h t 
          l = length aux

-- Função que agrupa os elementos, iguais a n, consecutivos da lista
groupAux :: Eq a => a -> [a] -> [a]
groupAux _ [] = [] 
groupAux n (h:t) | n == h = h : groupAux n t 
                 | otherwise = []

--12 
concat' :: [[a]] -> [a] 
concat' [] = []
concat' (h:t) = h ++ concat' t

--13
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

--14 
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' list = list : tails' (tail list)

--15                          
heads' :: [[a]] -> [a] 
heads' [] = []
heads' ([]:t) = heads' t 
heads' (h:t) = head h : heads' t 

--16                           
total' :: [[a]] -> Int 
total' [] = 0
total' (h:t) = length h + total' t 

--17 
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,_,c):t) = (a,c) : fun t 

--18
cola :: [(String,b,c)] -> String 
cola [] = []
cola ((st,_,_):t) = st ++ cola t 

--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade ano ida ((nome,nas):t) | (ano - nas) >= ida = nome : idade ano ida t 
                             | otherwise = idade ano ida t 

--20                                  
powerEnumFrom :: Int -> Int -> [Int]  
powerEnumFrom _ 1 = [1] 
powerEnumFrom n m | m > 1 = powerEnumFrom n (m - 1) ++ [n^(m-1)]
                  | otherwise = []

--21
isPrime :: Int -> Bool
isPrime n = n >= 2 && primeCheck n 2

-- Função que verifica se um número é primo 
primeCheck :: Int -> Int -> Bool
primeCheck n m | m^2 > n = True
               | mod n m == 0 = False
               | otherwise = primeCheck n (m + 1)

--22                           
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False 
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

--23 
isSuffixOf' :: Eq a => [a] -> [a] -> Bool 
isSuffixOf' [] _ = True 
isSuffixOf' _ [] = False 
isSuffixOf' l (h:t) = l == (h:t) || isSuffixOf' l t 

--24                
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool 
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False 
isSubsequenceOf' (x:xs) (y:ys) | x == y = isSubsequenceOf' xs ys 
                               | otherwise = isSubsequenceOf' (x:xs) ys

--25 
elemIndices' :: Eq a => a -> [a] -> [Int] 
elemIndices' ele list = elemIndicesAux ele list 0

elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux _ [] _ = []
elemIndicesAux ele (h:t) val | ele == h = val : elemIndicesAux ele t (val+1)
                             | otherwise = elemIndicesAux ele t (val+1) 

--26 
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) | h `elem` t = nub' t 
           | otherwise = h : nub' t 

--27 
delete' :: Eq a => a -> [a] -> [a] 
delete'  _ [] = []
delete' ele (h:t) | ele == h = t 
                  | otherwise = h : delete' ele t  

--28 
barraBarra :: Eq a => [a] -> [a] -> [a] 
barraBarra [] _ = []
barraBarra l [] = l 
barraBarra list (y:ys) = barraBarra (delete' y list) ys 

--29 
union' :: Eq a => [a] -> [a] -> [a] 
union' l [] = l 
union' l (h:t) | h `elem` l = union' l t 
               | otherwise = union' (l++[h]) t 

--30 
intersect' :: Eq a => [a] -> [a] -> [a] 
intersect' [] _ = [] 
intersect' (x:xs) list | x `elem` list = x : intersect' xs list 
                       | otherwise = intersect' xs list 

--31 
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (h:t) | x <= h = x : h : t 
                | otherwise = h : insert' x t 

--32 
unwords' :: [String] -> String 
unwords' [] = ""
unwords' [x] = x
unwords' (h:t) = h ++ " " ++ unwords' t 

--33 
unlines' :: [String] -> String 
unlines' [] = "" 
unlines' (h:t) = h ++ "\n" ++ unlines' t 

--34               
pMaior :: Ord a => [a] -> Int 
pMaior [_] = 0 
pMaior (h:t) | h >= (t !! pMaior t) = 0 
             | otherwise = 1 + pMaior t 

--35 
lookup' :: Eq a => a -> [(a,b)] -> Maybe b 
lookup' _ [] = Nothing 
lookup' ele ((a,b):t) | ele == a = Just b 
                      | otherwise = lookup' ele t 

--36
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = [] 
preCrescente [x] = [x]
preCrescente (h:b:t) | h <= b = h : preCrescente (b:t)
                     | otherwise = [h] 

--37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert' h (iSort t)

--38
menor :: String -> String -> Bool 
menor [] _ = True 
menor _ [] = True
menor (x:xs) (y:ys) | x == y = menor xs ys 
                    | x < y = True
                    | otherwise = False 

--39 
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False 
elemMSet ele ((a,_):t) | ele == a = True
                       | otherwise = elemMSet ele t 

--40 
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,n):t) = replicate n x ++ converteMSet t 

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]  
insereMSet ele ((a,b):t) | ele == a = (a,b+1):t 
                         | otherwise = (a,b) : insereMSet ele t

--42 
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
removeMSet _ [] = []
removeMSet ele ((a,b):t) | ele == a = if b <= 1 then t else (a,b-1):t 
                         | otherwise = (a,b) : removeMSet ele t

--43
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = [] 
constroiMSet (h:t) = insereMSet h (constroiMSet t)  

--44
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' (h:t) = case h of 
    Left x -> (x:l, r)
    Right x -> (l, x:r)
    where (l,r) = partitionEithers' t
 
--45
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Just x):t) = x : catMaybes' t 
catMaybes' (Nothing:t) = catMaybes' t 

--46 
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) = replicate (abs dx) sx ++ replicate (abs dy) sy
    where (dx,dy) = (x1-x2,y1-y2)
          sx = if dx<0 then Este else Oeste 
          sy = if dy<0 then Norte else Sul

--47                
hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False 
hasLoops p l | posicao p l == p = True 
             | otherwise = hasLoops p (init l)

-- Função que determina a posição final após efetuar uma lista de movimentos
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x, y) (Norte:t) = posicao (x, y + 1) t
posicao (x, y) (Sul:t) = posicao (x, y - 1) t
posicao (x, y) (Este:t) = posicao (x + 1, y) t
posicao (x, y) (Oeste:t) = posicao (x - 1, y) t 

--48
contaQuadrados :: [Rectangulo] -> Int 
contaQuadrados [] = 0 
contaQuadrados ((Rect (x,y) (x',y')):t) 
    | dx == dy = 1 + contaQuadrados t 
    | otherwise = contaQuadrados t 
    where (dx,dy) = (abs (x-x'),abs (y-y'))

--49
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x,y) (x',y')):t) =  dx * dy + areaTotal t 
        where (dx,dy) = (abs (x-x'),abs (y-y'))

--50
naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Avariado:t) = naoReparar t 
naoReparar (_:t) = 1 + naoReparar t

