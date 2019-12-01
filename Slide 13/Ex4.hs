{- Adicione comandos de condição e repetição à questão 1:
-}

data Cmd = Atr String Exp
        |  Seq Cmd Cmd
        |  Cnd Exp Cmd Cmd
        |  Rep Exp Cmd
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
avaliaCmd mem (Cnd e c1 c2)=if (avaliaExp mem e)/=0 then avaliaCmd mem c1 else avaliaCmd mem c2
avaliaCmd mem (Rep e c1) = if(avaliaExp mem e)/=0 then avaliaCmd (avaliaCmd mem c1) (Rep e c1) else mem
c2= Seq (Atr "x" (Num 1)) (Seq (Atr "y" (Num 2)) (Atr "z" ( Add (Var "x") (Var "y"))))
c3= Atr "x" (Add (Var "x") (Num 20))
c4= Seq (Atr "x" (Var "y")) (Seq (Atr "y" (Num 2)) (Atr "y" ( Sub (Var "x") (Var "y"))))
c5 = Atr "x" (Var "x")
c6= Atr "x" (Num 10)
c7= Cnd (Var "x") (Atr "y" (Num 10)) (Atr "z" (Num 2))
memo= [("y",10)]
c8 = Rep (Var "y") (Seq  (Atr "x" (Add (Var "y") (Var "x"))) (Atr "y" (Sub (Var "y") (Num 1))) )
main = print (avaliaCmd memo c8)
