
module Ex3 where 

import Data.List 
import Data.Char

data Movimento = Credito Float | Debito Float 
data Data = D Int Int Int 
data Extrato = Ext Float [(Data,String,Movimento)]  
    
ext :: Extrato 
ext = Ext 500 [(D 4 5 2001,"Deposito",Credito 300),
               (D 5 9 2003,"Propinas",Debito 100),
               (D 27 11 1999,"Financas",Debito 150),
               (D 13 3 2002,"Emprego",Credito 200)]

--a) 
eqData :: Data -> Data -> Bool 
eqData (D d1 m1 a1) (D d2 m2 a2) = d1 == d2 && m1 == m2 && a1 == a2 

instance Eq Data where 
    (==) = eqData

ordData :: Data -> Data -> Bool 
ordData (D d1 m1 a1) (D d2 m2 a2) = a2 > a1 || (a2 == a1 && m2 > m1) || (a2 == a1 && m2 == m1 && d2 >= d1)
                                  
instance Ord Data where 
    (<=) = ordData

--b) 
showData :: Data -> String 
showData (D d m a) = show a ++ "/" ++ show m ++ "/" ++ show d 

instance Show Data where 
    show = showData 

--c) 
ordena :: Extrato -> Extrato 
ordena (Ext ini list) = Ext ini listOrdenada
    where listOrdenada = sortOn fstTriple list 

-- Função que retorna o primeiro elemento de um triplo 
fstTriple :: (a,b,c) -> a 
fstTriple (x,y,z) = x 

--d)                               
showExtrato :: Extrato -> String 
showExtrato extrato = let (Ext ini list) = ordena extrato 
                      in "-----------------------------------------\n" ++ 
                         "Saldo Inicial: " ++ show ini ++ "\n" ++
                         "-----------------------------------------\n" ++ 
                         "Data        Descrição    Crédito   Débito\n" ++
                         "-----------------------------------------" ++ "\n" ++ 
                         concat (map showMovs list) ++ 
                         "-----------------------------------------" ++ "\n" ++ 
                         "Saldo Atual: " ++ show (saldoAtual extrato) ++ "\n" ++ 
                         "-----------------------------------------"

showMovs :: (Data,String,Movimento) -> String 
showMovs (date,desc,Credito x) = 
    show date ++ 
    (replicate (data_max - length (show date)) ' ') ++ 
    (map toUpper desc) ++ 
    (replicate (desc_max - length desc) ' ') ++ 
    show x ++ "\n"
    where data_max = 12
          desc_max = 13
showMovs (date,desc,Debito x) = 
    show date ++ 
    (replicate (data_max - length (show date)) ' ') ++ 
    (map toUpper desc) ++ 
    (replicate (desc_max - length desc) ' ') ++ 
    replicate cred_max ' ' ++ 
    show x ++ "\n"
    where data_max = 12
          desc_max = 12
          cred_max = 11

saldoAtual :: Extrato -> Float
saldoAtual (Ext ini list) = ini + sum listMovs 
    where listMovs = map (\ (_,_,mov) -> case mov of 
                                              Credito x -> x
                                              Debito y -> (-y)) list 

instance Show Extrato where 
    show = showExtrato 

