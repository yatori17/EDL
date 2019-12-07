{-

    Crie uma nova representação de árvores que guarde valores nas folhas em vez de guardar nos galhos.

    Redefina todas as funções do exercício 2 para funcionarem com essa nova representação.

-}
data Arvore = Folha Int| Galho Arvore Arvore
    deriving Show
a1 = Galho (Galho (Folha 1) (Folha 2)) (Galho (Folha 3) (Folha 4))
{- a2 = Galho 1 (Galho 1 (Galho 1 Folha Folha) Folha) Folha
a3 = Galho 0 Folha (Galho 0 (Galho 0 Folha Folha) (Galho 0Folha Folha)) -}

folhas :: Arvore -> Int
folhas (Folha _)= 1
folhas (Galho a b)= folhas a + folhas b

altura :: Arvore -> Int
altura (Folha _ )= 0
altura (Galho a b) = 1 + maior (altura a) (altura b) where 
    maior a b= if a> b then a else b
espelho :: Arvore -> Arvore
espelho (Folha c)= (Folha c)
espelho (Galho a b) = Galho (espelho b) (espelho a)

soma :: Arvore -> Int
soma (Folha c)= c
soma (Galho a b)= soma a + soma b 

dobra :: Arvore -> Arvore
dobra (Folha c) = (Folha (2*c))
dobra (Galho a b ) = Galho (dobra a ) (dobra b)

possui :: Arvore -> Int -> Bool
possui (Folha x) val = x== val
possui (Galho a b) val =(possui a val || possui b val)

{- possui2 :: Arvore -> Int -> Bool
possui2 Folha x = False
possui2 (Galho c a b) val = if(c== val ) then True else (if val < c then possui2 a val else possui2 b val)
 -}
maximo :: Arvore -> Int
maximo (Folha x) = x
maximo (Galho a b) = max (maximo a) (maximo b)

insere :: Arvore -> Int -> Arvore
insere (Folha x) val = if(val < x) then (Galho (Folha val) (Folha x)) else (Galho (Folha x) (Folha val)) 
insere (Galho a b) val = if (val <(folhas a)) then Galho (insere a val) b else Galho a (insere b val)
main = print ( dobra a1)