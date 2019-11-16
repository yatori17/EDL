data Cmd = Atr String Exp
        |  Seq Cmd Cmd
        deriving Show 
data Exp = Num Int
        |   Add Exp Exp 
        |   Sub Exp Exp
        |   Var String
        deriving Show
a= Add (Num 1) (Num 2)
c0= Atr "x" (Num 1)
c1= Atr "x" (Add (Num 1) (Sub (Var "x") (Num 20)))

memory = []
type Mem = [(String,Int)]
avaliaExp ::Mem ->Exp -> Int
avaliaExp mem (Num j)= j
avaliaExp mem (Add a b) = (avaliaExp mem a) + (avaliaExp  mem b)
avaliaExp mem (Sub a b) = (avaliaExp mem a) - (avaliaExp  mem b) 
avaliaExp mem (Var id) = consulta mem id

consulta ::Mem -> String -> Int
consulta [] id = 0
consulta ((id',aux'):l) id = if id == id' then aux' else consulta l id

escreve:: Mem -> String -> Int -> Mem
escreve mem id exp = (id,exp):mem
avaliaCmd:: Mem -> Cmd -> Mem
avaliaCmd mem (Atr id exp) = escreve mem id (avaliaExp mem exp)
avaliaCmd mem (Seq a b) = avaliaCmd mem' b where
                          mem'= avaliaCmd mem a
c2= Seq (Atr "x" (Num 1)) (Seq (Atr "y" (Num 2)) (Atr "z" ( Add (Var "x") (Var "y"))))
c3= Atr "x" (Add (Var "x") (Num 20))
c4= Seq (Atr "x" (Var "y")) (Seq (Atr "y" (Num 2)) (Atr "y" ( Sub (Var "x") (Var "y"))))
c5 = Atr "x" (Var "x")
main = print (avaliaCmd memory  c5)
