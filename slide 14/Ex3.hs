data Cmd = Atr String Exp
        |  Seq Cmd Cmd
        |  Dcl String
        |  Nulo
        deriving Show 
data Exp = Num Int
        |   Add Exp Exp 
        |   Sub Exp Exp
        |   Var String
        deriving Show
c0= Seq (Dcl "x") ( Atr "x" (Num 1))
c1 = Atr "x" (Num 10)
c2= Dcl "x"

verificaExp:: [String] -> Exp -> Bool
verificaExp [] (exp) = False
verificaExp arr (Var id')= elem id' arr
verificaExp arr (Add a b) = verificaExp arr a && verificaExp arr b
verificaExp arr (Sub a b) = verificaExp arr a && verificaExp arr b
verificaExp _ _ = True

verificaCmd:: [String] -> Cmd -> ([String], Bool)
verificaCmd arr (Atr id exp) = (arr, (elem id arr) && (verificaExp arr exp))
verificaCmd arr (Dcl id) = (id:arr, True)
verificaCmd arr (Seq a b ) = (v2, b1 && b2) where
            (v1,b1)= verificaCmd arr a
            (v2,b2)= verificaCmd v1 b
---- 2

type Mem = [(String,Int)]
consulta :: Mem -> String -> Int
consulta [] id = 0
consulta ((id,aux):l) id' = if id == id' then aux else consulta l id'

escreve:: Mem -> String -> Int -> Mem
escreve mem id inteiro = (id,inteiro):mem
--verificaProg :: Cmd -> Bool
---Avalia progrmas sem memóória(?)

avaliaExp ::Mem -> Exp -> Int
avaliaExp mem (Num j)= j
avaliaExp mem (Add a b) = (avaliaExp mem a) + (avaliaExp  mem b)
avaliaExp mem (Sub a b) = (avaliaExp mem a) - (avaliaExp  mem b) 
avaliaExp mem (Var id) = consulta mem id

avaliaCmd:: Mem -> Cmd -> Mem
avaliaCmd mem (Atr id exp) = escreve mem id (avaliaExp mem exp)
avaliaCmd mem (Seq a b) = avaliaCmd mem' b where
                          mem'= avaliaCmd mem a

--avaliaCmd (Atr id exp) = 
--avaliaProg :: Cmd -> Maybe Int

memory=[]
--2.Crie uma função verificaProg :: Cmd -> Bool, que recebe um comando e retorna se o comando é válido de acordo com a questão 1
verificaProg :: Cmd -> Bool
verificaProg (Atr id exp) =verificaExp memory exp
verificaProg (Seq a b ) = verificaProg a && verificaProg b
verificaProg (Dcl id ) = True

--avaliaProg :: Cmd -> Maybe Int 
--avaliaProg c = if verificaProg c1 == False then Nothing else Just (consulta mem' "ret") where 
 -- mem' = avaliaCmd [] c1


eliminaDcl :: Cmd -> Cmd
eliminaDcl (Dcl id) = (Nulo)
eliminaDcl (Nulo) = (Nulo)
eliminaDcl (Atr id exp)= (Atr id exp)
eliminaDcl (Seq a b ) = (Seq (eliminaDcl a) (eliminaDcl b))
--Altere a função avaliaProg da questão 2 de modo que ela não mais avalie comandos Dcl inócuos.

c3= Seq ( Atr "x" (Num 10)) (Dcl "x")
main = print ( eliminaDcl c0)
