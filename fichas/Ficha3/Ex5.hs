
module Ex5 where 

data Movimento = Credito Float | Debito Float 
               deriving Show 

data Data = D Int Int Int 
          deriving Show

data Extrato = Ext Float [(Data, String, Movimento)]
             deriving Show

--a) 
extValor :: Extrato -> Float -> [Movimento]
extValor (Ext _ []) _ = []
extValor (Ext s ((_,_,mov):t)) n = case mov of 
    Credito v -> if v >= n then mov : extValor (Ext s t) n else extValor (Ext s t) n
    Debito v  -> if v >= n then mov : extValor (Ext s t) n else extValor (Ext s t) n 

--b) 
filtro :: Extrato -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext s ((d,desc,mov):t)) l | desc `elem` l = (d,mov) : filtro (Ext s t) l 
                                  | otherwise = filtro (Ext s t) l 

--c) 
creDeb :: Extrato -> (Float,Float) 
creDeb (Ext _ []) = (0,0)
creDeb (Ext s ((_,_,mov):t)) = case mov of 
    Credito n -> (c + n,d) 
    Debito n  -> (c,d + n) 
    where (c,d) = creDeb (Ext s t)

--d) 
saldo :: Extrato -> Float 
saldo (Ext s list) = s + c - d
        where (c,d) = creDeb (Ext s list)

