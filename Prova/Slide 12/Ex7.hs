data Exp = Num Int
        |  Add Exp Exp
        |  Sub Exp Exp
        |  Var String
        deriving Show
data Cmd = Atr String Exp
        |  Seq Cmd Cmd
        deriving Show
{-
type Mem = [(String,Int)]
    -- Associa um identificador a um valor inteiro
    -- O valor mais recente é adicionado no início
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

avaliaCmd :: Mem -> Cmd -> Mem
avaliaCmd mem (Atr id e)= escreve mem id (avaliaExp mem e)
avaliaCmd mem (Seq a b)= avaliaCmd mem' a where
    mem' = avaliaCmd mem b


main = print (avaliaCmd [] c0)