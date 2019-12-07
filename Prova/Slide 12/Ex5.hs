data Exp = Num Int
        |  Add Exp Exp
        |  Sub Exp Exp
        |  And Exp Exp
        |  Or  Exp Exp
        |  Not Exp
        deriving Show
e0 = Add (Num 1) (Num 2) 
e1 = Add (Add (Add (Num 1) (Num 2)) (Num 3)) (Num 4)
e2 = Add (Num 1) (Add (Num 2) (Add (Num 3) (Num 4)))
e3 = Sub (Sub (Sub (Num 1) (Num 2)) (Num 3)) (Num 4)
e4 = Sub (Num 1) (Sub (Num 2) (Sub (Num 3) (Num 4)))
e5 = Sub (Sub (Num 1) (Num 2)) (Sub (Num 3) (Num 4))
{-

    Adicione expressões booleanas (and, or, not) ao exercício 1.
        Redefina a representação.
        Redefina as funções avalia.
        Baseie-se no comportamento da linguagem C.
        Qual decisão de projeto foi necessária, por quê ela foi necessária e qual foi a sua escolha?
        A função avalia não pode retornar inteiro e depois boolean! A decisão foi usar a msm que em c
        QUando uma expressão é falsa, é atribuido o zero a expressão e quando ela é verdadeira, todos os outros reais!

        -}


avalia :: Exp -> Int
avalia (Num id)= id
avalia (Add a b) = avalia a + avalia b
avalia (Sub a b)=  avalia a - avalia b
avalia (And a b)= if((avalia a) * (avalia b))/=0 then 1 else 0
avalia (Or a b)= if((avalia a) + (avalia b))/=0 then 1 else 0
avalia (Not a)= if(avalia a)==0 then 1 else 0



avalia':: Exp -> Exp
avalia' (Num id)= (Num id)
avalia' (Add a b)= Num (f (avalia' a)(avalia' b)) where
    f (Num x) (Num y)= x + y
avalia' (Sub a b)= Num (f (avalia' a)(avalia' b)) where
    f (Num x) (Num y)= x - y
avalia' (And a b)= Num (f (avalia' a)(avalia' b)) where
    f (Num x) (Num y)= if(x * y)/=0 then 1 else 0
avalia' (Or a b)= Num (f (avalia' a)(avalia' b)) where
    f (Num x) (Num y)= if(x + y)/=0 then 1 else 0
avalia' (Not a )= Num (f (avalia' a)) where
    f (Num x) = if x==0 then 1 else 0


main = print (avalia' e5)