
module Ex2 where 

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

conta :: Exp Int 
conta = Mais (Const 3) (Menos (Const 10) (Const 22))

conta' :: Exp Int 
conta' = Mult (Const 3) (Menos (Const 2) (Const 5))

--a)
infixa :: Show a => Exp a -> String 
infixa (Mais e1 e2) = "(" ++ infixa e1 ++ " + " ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = "(" ++ infixa e1 ++ " - " ++ infixa e2 ++ ")"
infixa (Mult e1 e2) = "(" ++ infixa e1 ++ " x " ++ infixa e2 ++ ")"
infixa (Simetrico e) = "(-" ++ infixa e ++ ")"
infixa (Const x) = show x

instance (Show a) => Show (Exp a) where
    show = infixa  

--b) 
calcula :: Num a => Exp a -> a 
calcula (Mais arv1 arv2) = calcula arv1 + calcula arv2 
calcula (Menos arv1 arv2) = calcula arv1 - calcula arv2 
calcula (Mult arv1 arv2) = calcula arv1 * calcula arv2
calcula (Const x) = x 
calcula (Simetrico x) = - calcula x

expEq :: (Eq a, Num a) => Exp a -> Exp a -> Bool
expEq e1 e2 = calcula e1 == calcula e2 

instance (Eq a, Num a) => Eq (Exp a) where 
    (==) = expEq

-- c) 
somaExp :: Num a => Exp a -> Exp a -> Exp a 
somaExp = Mais

multExp :: Num a => Exp a -> Exp a -> Exp a
multExp = Mult

subtExp :: Num a => Exp a -> Exp a -> Exp a 
subtExp = Menos 

absExp :: (Num a, Ord a) => Exp a -> Exp a 
absExp e = Const (abs x)
    where x = calcula e

signumExp :: (Num a) => Exp a -> Exp a 
signumExp exp = Const $ signum (calcula exp)

fromIntegerExp :: Num a => Integer -> Exp a
fromIntegerExp n = Const (fromInteger n)

instance (Num a, Ord a) => Num (Exp a) where 
    (+) = somaExp 
    (*) = multExp 
    (-) = subtExp
    abs = absExp 
    signum = signumExp 
    fromInteger = fromIntegerExp 

