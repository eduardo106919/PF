
module Ex1 where 

data Frac = F Integer Integer 


--a)                            
normaliza :: Frac -> Frac 
normaliza (F x y) | y < 0 = F (div (-x) m) (div y m)
                  | otherwise = F (div x m) (div y m)
    where m = mdc x y 

mdc :: Integer -> Integer -> Integer 
mdc x 0 = x 
mdc 0 y = y 
mdc x y = mdc y (x `mod` y) 

--b)
fracEq :: Frac -> Frac -> Bool 
fracEq f1 f2 = x1 == x2 && y1 == y2
    where F x1 y1 = normaliza f1 
          F x2 y2 = normaliza f2 

instance Eq Frac where 
    (==) = fracEq 
 
--c) 
fracOrd :: Frac -> Frac -> Bool
fracOrd f f' = x*y' <= x'*y
    where (F x y) = normaliza f 
          (F x' y') = normaliza f'

instance Ord Frac where 
    (<=) = fracOrd 

--d) 
fracShow :: Frac -> String 
fracShow f = show x ++ "/" ++ show y 
    where (F x y) = normaliza f

instance Show Frac where 
    show = fracShow 

--e) 
somaFrac :: Frac -> Frac -> Frac 
somaFrac f f' = normaliza (F (x*y' + x'*y) (y*y'))
    where (F x y) = normaliza f 
          (F x' y') = normaliza f' 

multFrac :: Frac -> Frac -> Frac 
multFrac (F x1 y1) (F x2 y2) = normaliza (F (x1*x2) (y1*y2))

absFrac :: Frac -> Frac 
absFrac f = F (abs x) (abs y) 
    where (F x y) = normaliza f

negateFrac :: Frac -> Frac 
negateFrac f = F (negate x) y 
    where (F x y) = normaliza f 

signumFrac :: Frac -> Frac 
signumFrac f = normaliza $ F (signum x) (signum y)
    where F x y = normaliza f  

fromIntegerFrac :: Integer -> Frac 
fromIntegerFrac x = F x 1 

instance Num Frac where 
    (+) = somaFrac 
    (*) = multFrac 
    abs = absFrac 
    negate = negateFrac
    signum = signumFrac 
    fromInteger = fromIntegerFrac 

--f) 
seleciona :: Frac -> [Frac] -> [Frac] 
seleciona f = filter (>dobroF) 
    where dobroF = 2*f 

