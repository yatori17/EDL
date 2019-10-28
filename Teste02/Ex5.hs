data Exp = Num Int
  |        Add Exp Exp
  |        Sub Exp Exp
  |        Mul Exp Exp
  |        Div Exp Exp
  |        And Exp Exp
  |        Or Exp Exp
  |        Not Exp
  deriving Show

{-e0= Num 1
--1 + 10 - 20
e1= Sub (Add (Num 1) (Num 10)) (Num 20)
--1 + (10 - 20)
e2= Add (Num 1) (Sub (Num 10) (Num 20))
--(5 + 5) - (5 - 5)
e3= Sub (Add (Num 5) (Num 5)) (Sub (Num 5) (Num 5))
-}
e0 = Add (Num 1) (Num 2)
e1 = Add (Add (Add (Num 1) (Num 2)) (Num 3)) (Num 4)
e2 = Add (Num 1) (Add (Num 2) (Add (Num 3) (Num 4)))
e3 = Sub (Sub (Sub (Num 1) (Num 2)) (Num 3)) (Num 4)
e4 = Sub (Num 1) (Sub (Num 2) (Sub (Num 3) (Num 4)))
e5 = Sub (Sub (Num 1) (Num 2)) (Sub (Num 3) (Num 4))

conta:: Exp -> String
conta (Num a) = show a
conta (Add (a) (b)) = "(" ++ conta (a) ++ " + " ++ conta (b) ++ ")"
conta (Sub (a) (b)) = "(" ++ conta (a) ++ " - " ++ conta (b) ++ ")"

avalia:: Exp-> Int
avalia (Num a) = a
avalia (Add a b)= avalia a + avalia b
avalia (Sub a b) = avalia a - avalia b
avalia (Mul a b) = avalia a * avalia b
avalia (Div a b) = avalia a `div` avalia b
avalia (And a b) = if(avalia a >= 1 && avalia  b >=1) then 1 else 0
avalia (Or a b) = if (avalia a>=1 || avalia b>=1 ) then 1 else 0
avalia (Not a) = if(avalia a>=1) then 0 else 1
converte :: Int -> Exp
converte (x) = Num x
{-
avalia' :: Exp -> Exp
avalia' (Num e1) = (Num e1)
avalia' (Add e1 e2) = converte(avalia (e1) + avalia (e2))
avalia' (Sub e1 e2) = converte(avalia (e1) - avalia (e2))
avalia' (Mul a b) = converte(avalia (a) * avalia (b))
avalia' (Div a b) = converte(avalia (a) `div` avalia (b)) -}


main = print (avalia (And (Num 1) (Num (-1))))

