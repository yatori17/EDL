data Exp = Num Int
        |  Add Exp Exp
        |  Sub Exp Exp
        |  Var String
        |  Mul Exp Exp
        deriving Show
data Cmd = Atr String Exp
        |  Seq Cmd Cmd
        |  Cnd Exp Cmd Cmd 
        |  Rep Exp Cmd
        deriving Show
{-
Adicione comandos de condição e repetição à questão 7:

data Cmd = Atr String Exp
         | Seq Cmd Cmd
         | Cnd Exp Cmd Cmd
         | Rep Exp Cmd

    Reimplemente a função avaliaCmd para tratar os novos comandos.

    Teste programas com as especificações a seguir:
        Calcule a soma de x até y (x + x+1 + x+2 + ... + y).
        Calcule a soma dos quadrados de 1 até 10 (1*1 + 2*2 + ... + 10*10).

-}
type Mem = [(String,Int)]
e0 = Add (Num 1) (Num 2) 
e1 = Add (Add (Add (Num 1) (Num 2)) (Num 3)) (Num 4)
e2 = Add (Num 1) (Add (Num 2) (Add (Num 3) (Num 4)))
e3 = Sub (Sub (Sub (Num 1) (Num 2)) (Num 3)) (Num 4)
e4 = Sub (Num 1) (Sub (Num 2) (Sub (Num 3) (Num 4)))
e5 = Sub (Sub (Num 1) (Num 2)) (Sub (Num 3) (Num 4))

c0= Atr "x" (Add (Num 1)(Num 2))
c1= Seq (Atr "x" (Num 10)) (Atr "x" (Num 1))
c2 = Seq (Atr "x" (Num 1)) (Seq (Atr "y" (Add (Var "x") (Num 1))) (Atr "x" (Var "y")))

consulta :: Mem -> String -> Int
consulta [] id = 0
consulta ((id,val):l) id' = if(id' == id) then val else consulta l id'

escreve :: Mem -> String -> Int -> Mem
escreve mem id inteiro = (id,inteiro):mem

avaliaExp :: Mem -> Exp -> Int
avaliaExp mem (Num id)= id
avaliaExp mem (Add a b) = (avaliaExp mem  a) + (avaliaExp mem b)
avaliaExp mem (Sub a b)=  avaliaExp mem  a - avaliaExp mem b
avaliaExp mem (Var id) = consulta mem id
avaliaExp mem (Mul a b)= avaliaExp mem a * avaliaExp mem b

avaliaCmd :: Mem -> Cmd -> Mem
avaliaCmd mem (Atr id e)= escreve mem id (avaliaExp mem e)
avaliaCmd mem (Seq a b)= avaliaCmd mem' a where
    mem' = avaliaCmd mem b
avaliaCmd mem (Cnd exp b1 b2)= if (avaliaExp mem exp)/=0 then avaliaCmd mem b1 else avaliaCmd mem b2
avaliaCmd mem (Rep exp b1) = if(avaliaExp mem exp)/=0 then avaliaCmd (avaliaCmd mem b1) (Rep exp b1) else mem

memory = [("x",5),("y",10)]
soma = Seq (Rep (Var "z") (Seq (Atr "z" (Sub (Var "z")(Num 1)))(Atr "x" (Add (Var "x") (Var "z"))))) (Atr "z" (Sub (Var "y") (Var "x"))) 
mult = Seq (Rep (Sub (Num 11)(Var "i")) (Seq  (Atr "i" (Add (Var "i") (Num 1)))  ( Atr "aux" (Mul (Var "i")(Var "i"))))) (Atr "i" (Num 1))
main = print (avaliaCmd memory mult)