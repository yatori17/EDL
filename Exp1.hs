{-Considere a seguinte representação para árvores binárias:

data Arvore = Folha | Galho Int Arvore Arvore

    Redefina todas as funções do exercício 1 para funcionarem com essa nova representação
-}
data Arvore = Folha | Galho Int Arvore Arvore
  deriving Show


a1= Galho 5 (Galho 1 Folha Folha)(Galho 6 Folha Folha)
a2= Galho 4 ( Galho  2 (Galho 1 Folha Folha) Folha) Folha
a3= Galho 3 Folha( Galho 3 (Galho 3 Folha Folha) (Galho 3  Folha Folha))

folhas:: Arvore-> Int
folhas Folha= 1
folhas (Galho _ a1 a2)= folhas a1 + folhas a2

altura:: Arvore->Int
altura Folha=0
altura (Galho _ a1 a2) = 1 + maior (altura a1) (altura a2)
  where 
    maior:: Int-> Int-> Int
    maior a b= if a> b then a else b
espelho:: Arvore -> Arvore
espelho Folha = Folha
espelho (Galho x a1 a2)= (Galho x (espelho a2)  (espelho a1))


{- Defina a função soma que recebe uma árvore e retorna a soma de todos os valores guardados nos seus galhos.-}

soma:: Arvore -> Int
soma Folha =0
soma (Galho x a1 a2)= x + soma a1 + soma a2

{-
    Defina a função dobra que recebe uma árvore e retorna uma nova árvore com todos os valores dos galhos originais duplicados.
    -}

dobra:: Arvore-> Arvore
dobra Folha= Folha
dobra (Galho x a1 a2)=Galho (n x) (dobra a1) (dobra a2)
  where
    n:: Int-> Int
    n x= x * 2

{-
Defina uma função possui que recebe uma árvore e um valor e retorna se algum galho da árvore guarda esse valor.
 
possui:: Arvore-> Int-> Bool
possui Folha x= False
possui (Galho x a1 a2) y= if x==y then True else  (possui a1 y || possui a2 y)
Considere ávores binárias de busca, ou seja, árvores em que os galhos à esquerda sempre guardam valores menores ou iguais que os galhos à direita.
        Redefina a função possui de maneira mais eficiente que o exercício anterior.-}
possui:: Arvore-> Int-> Bool
possui Folha x= False
possui (Galho x a1 a2) y= if x==y then True else if  x<y then possui a1 y else possui a2 y
{-  Defina a função maximo que recebe uma árvore e retorna o seu maior valor. -}

maximo :: Arvore -> Int
maximo Folha= 0
maximo (Galho x a1 a2)= if ((maximo a2)==0) then x else maximo a2

{- Defina a função insere que recebe uma árvore e um valor e retorna uma nova árvore com esse novo valor, respeitando a propriedade de árvore de busca.-}
a4= Galho 5 ( Galho 1 Folha Folha) (Galho 8 (Galho 7 Folha Folha) (Galho 10 Folha Folha))
insere:: Arvore-> Int-> Arvore
insere Folha x= Galho x Folha Folha 
insere (Galho x a1 a2) y = if(x == y) then (Galho x a1 a2)
 else if(x>y) then (Galho x (insere a1 y) a2)
  else if(x<y) then (Galho x a1 (insere a2 y) )
  else (Galho y a1 a2)
main= print a1
