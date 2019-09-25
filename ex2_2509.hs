--2.

data Arvore= Folha | Galho Int (Arvore ) (Arvore )   
  deriving Show

a0= Galho 10 Folha Folha
a1= Galho 10 a0 a0
a2= Galho 1 ( Galho 0 ( a0) Folha) Folha
a3= Galho 3 Folha ( Galho 2 (a0) (a0))
a4=Folha

folhas:: Arvore -> Int
folhas Folha = 1
folhas (Galho _ a1 a2) = folhas a1+ folhas a2

altura:: Arvore  -> Int
--altura three = if folhas three ==1 then 0 else 1
altura Folha = 0
altura (Galho _ a1 a2) = 1 + maior (altura a1) (altura a2)
  where 
    maior:: Int -> Int -> Int
    maior x y= if x> y then x else y

espelho:: Arvore -> Arvore
espelho Folha= Folha
espelho (Galho x a1 a2)= (Galho x (espelho a2) (espelho a1))



--2.
soma:: Arvore-> Int
soma Folha =0
soma (Galho x a1 a2)= x+ soma a1+ soma a2

dobra:: Arvore -> Arvore
dobra Folha= Folha
dobra (Galho x a1 a2)= (Galho (n x) (dobra a1) (dobra a2))
  where 
  n :: Int -> Int
  n x = 2*x

possui:: Arvore-> Int-> Bool
possui Folha x =False
possui (Galho n a1 a2) x = if n == x then True else (possui a1 || possui a2 )
main= print (possui a1)
