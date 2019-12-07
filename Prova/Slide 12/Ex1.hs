data Exp = Num Int
        |  Add Exp Exp
        |  Sub Exp Exp
        deriving Show
e0 = Add (Num 1) (Num 2) 
e1 = Add (Add (Add (Num 1) (Num 2)) (Num 3)) (Num 4)
e2 = Add (Num 1) (Add (Num 2) (Add (Num 3) (Num 4)))
e3 = Sub (Sub (Sub (Num 1) (Num 2)) (Num 3)) (Num 4)
e4 = Sub (Num 1) (Sub (Num 2) (Sub (Num 3) (Num 4)))
e5 = Sub (Sub (Num 1) (Num 2)) (Sub (Num 3) (Num 4))


avalia :: Exp -> Int
avalia (Num id)= id
avalia (Add a b) = avalia a + avalia b
avalia (Sub a b)=  avalia a - avalia b

avalia':: Exp -> Exp
avalia' (Num id)= (Num id)
avalia' (Add a b)= Num (f (avalia' a)(avalia' b)) where
    f (Num x) (Num y)= x + y
avalia' (Sub a b)= Num (f (avalia' a)(avalia' b)) where
    f (Num x) (Num y)= x - y

main = print (avalia' e5)