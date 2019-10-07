{-

    Crie uma nova representação de árvores que guarde valores nas folhas em vez de guardar nos galhos.

    Redefina todas as funções do exercício 2 para funcionarem com essa nova representação.

-}
data Arvore =Folha Int | Galho Arvore Arvore 
  deriving Show


a1= Galho (Galho (Folha 1) (Folha 2))(Galho (Folha 3) (Folha 4))
a2= Galho ( Galho  (Galho (Folha 1) (Folha 2)) (Folha 3) ) (Folha 4)
a3= Galho (Folha 1) ( Galho (Galho (Folha 2) (Folha 3)) (Galho (Folha 4) (Folha 5)))

folhas:: Arvore-> Int
folhas (Folha _) = 1
folhas (Galho a1 a2)= folhas a1 + folhas a2

altura:: Arvore->Int
altura (Folha _)=0
altura (Galho a1 a2) = 1 + maior (altura a1) (altura a2)
  where 
    maior:: Int-> Int-> Int
    maior a b= if a> b then a else b
espelho:: Arvore -> Arvore
espelho (Folha x)= (Folha x)
espelho (Galho a1 a2)= (Galho (espelho a2)  (espelho a1))


{- Defina a função soma que recebe uma árvore e retorna a soma de todos os valores guardados nos seus galhos.-}

soma:: Arvore -> Int
soma (Folha x) = x
soma (Galho a1 a2)=  soma a1 + soma a2

{-
    Defina a função dobra que recebe uma árvore e retorna uma nova árvore com todos os valores dos galhos originais duplicados.
    -}

dobra:: Arvore-> Arvore
dobra (Folha x)= (Folha (n x))
  where
    n:: Int-> Int
    n x= x * 2
dobra (Galho a1 a2)= (Galho (dobra a1) (dobra a2))


{-
Defina uma função possui que recebe uma árvore e um valor e retorna se algum galho da árvore guarda esse valor.
 
possui:: Arvore-> Int-> Bool
possui Folha x= False
possui (Galho x a1 a2) y= if x==y then True else  (possui a1 y || possui a2 y)
Considere ávores binárias de busca, ou seja, árvores em que os galhos à esquerda sempre guardam valores menores ou iguais que os galhos à direita.
        Redefina a função possui de maneira mais eficiente que o exercício anterior.-}
possui:: Arvore-> Int-> Bool
possui (Folha x) y= x==y
possui (Galho a1 a2) y= (possui a1 y) || (possui a2 y)
{-  Defina a função maximo que recebe uma árvore e retorna o seu maior valor. -}

maximo :: Arvore -> Int
maximo (Folha x) = x
maximo (Galho a1 a2)= if (maximo a1) > (maximo a2) then (maximo a1) else (maximo a2)

{- Defina a função insere que recebe uma árvore e um valor e retorna uma nova árvore com esse novo valor, respeitando a propriedade de árvore de busca.-}
--a4= Galho 5 ( Galho 1 Folha Folha) (Galho 8 (Galho 7 Folha Folha) (Galho 10 Folha Folha))
insere:: Arvore-> Int-> Arvore
insere (Folha x) y = if( y > x) then (Galho (Folha x) (Folha y)) else  (Galho (Folha y) (Folha x))
insere (Galho a1 a2) x= (Galho (insere a1 x) (insere a2 x)) 
main= print a1
