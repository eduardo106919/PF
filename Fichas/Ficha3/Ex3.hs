
module Ex3 where 
import Test.HUnit

data Contacto = Casa Integer 
              | Trab Integer
              | Tlm Integer 
              | Email String 
              deriving Show 

type Nome = String 
type Agenda = [(Nome,[Contacto])] 

--a)
acrescEmail :: Nome -> String -> Agenda -> Agenda 
acrescEmail n e [] = [(n,[Email e])] 
acrescEmail n e ((nome,l):t) | n == nome = (nome,Email e :l) : t 
                             | otherwise = (nome,l) : acrescEmail n e t
--b)                            
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing 
verEmails n ((nome,l):t) | n == nome = Just (emails l) 
                         | otherwise = verEmails n t 

-- Função auxiliar de verEmails que seleciona os emails de uma lista de contactos
emails :: [Contacto] -> [String]
emails [] = []
emails (h:t) = case h of 
    Email s -> s : emails t 
    _ -> emails t                 

--c)       
consTelefs :: [Contacto] -> [Integer] 
consTelefs [] = []
consTelefs (h:t) = case h of 
    Tlm n  -> n : consTelefs t 
    Trab n -> n : consTelefs t 
    Casa n -> n : consTelefs t 
    _      -> consTelefs t 

--d) 
casa :: Nome -> Agenda -> Maybe Integer 
casa _ [] = Nothing 
casa nome ((n,l):t) | nome == n = telefone l 
                    | otherwise = casa nome t 

-- Função auxiliar de casa que seleciona o número de telefone de Casa 
telefone :: [Contacto] -> Maybe Integer 
telefone [] = Nothing 
telefone (h:t) = case h of 
    Casa n -> Just n 
    _ -> telefone t 

