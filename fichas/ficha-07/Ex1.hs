
module Ex1 where 

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

conta :: ExpInt 
conta = Mais 
          (Menos 
                (Const 7) 
                (Const 4))
          (Mult 
                (Simetrico (Const 2)) 
                (Mult 
                     (Const 5) 
                     (Const 5)))

conta' :: ExpInt 
conta' = Mais (Const 3) (Menos (Const 2) (Const 5))

--a)
calcula :: ExpInt -> Int 
calcula (Mais e1 e2) = calcula e1 + calcula e2 
calcula (Menos e1 e2) = calcula e1 - calcula e2 
calcula (Mult e1 e2) = calcula e1 * calcula e2
calcula (Const x) = x 
calcula (Simetrico x) = - calcula x

--b) 
infixa :: ExpInt -> String 
infixa (Mais e1 e2) = "(" ++ infixa e1 ++ " + " ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = "(" ++ infixa e1 ++ " - " ++ infixa e2 ++ ")"
infixa (Mult e1 e2) = "(" ++ infixa e1 ++ " x " ++ infixa e2 ++ ")"
infixa (Simetrico e) = "(-" ++ infixa e ++ ")"
infixa (Const x) = show x

--c)                                           
posfixa :: ExpInt -> String 
posfixa (Const x) = show x 
posfixa (Simetrico e) = posfixa e ++ " (-)"
posfixa (Mais e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " +" 
posfixa (Menos e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " -" 
posfixa (Mult e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " x" 

