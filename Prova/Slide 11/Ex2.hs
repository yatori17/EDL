{-Considere a seguinte representação para árvores binárias:

data Arvore = Folha | Galho Int Arvore Arvore

    Redefina todas as funções do exercício 1 para funcionarem com essa nova representação.

    Defina a função soma que recebe uma árvore e retorna a soma de todos os valores guardados nos seus galhos.

    Defina a função dobra que recebe uma árvore e retorna uma nova árvore com todos os valores dos galhos originais duplicados.

    Defina uma função possui que recebe uma árvore e um valor e retorna se algum galho da árvore guarda esse valor.

    Considere ávores binárias de busca, ou seja, árvores em que os galhos à esquerda 
    sempre guardam valores menores ou iguais que os galhos à direita.
        Redefina a função possui de maneira mais eficiente que o exercício anterior.
        Defina a função maximo que recebe uma árvore e retorna o seu maior valor.
        Defina a função insere que recebe uma árvore e um valor e retorna uma nova 
        árvore com esse novo valor, respeitando a propriedade de árvore de busca.
-}
data Arvore = Folha | Galho Int Arvore Arvore
    deriving Show
a1 = Galho 3 (Galho 2 Folha Folha) (Galho 3 Folha Folha)
a2 = Galho 1 (Galho 1 (Galho 1 Folha Folha) Folha) Folha
a3 = Galho 0 Folha (Galho 0 (Galho 0 Folha Folha) (Galho 0Folha Folha))

folhas :: Arvore -> Int
folhas (Folha)= 1
folhas (Galho _ a b)= folhas a + folhas b

altura :: Arvore -> Int
altura Folha = 0
altura (Galho _ a b) = 1 + maior (altura a) (altura b) where 
    maior a b= if a> b then a else b
espelho :: Arvore -> Arvore
espelho Folha = Folha
espelho (Galho c a b) = Galho c (espelho b) (espelho a)

soma :: Arvore -> Int
soma Folha = 0
soma (Galho valor a b)= valor + soma a + soma b 

dobra :: Arvore -> Arvore
dobra Folha = Folha
dobra (Galho valor a b ) = Galho (valor*2) (dobra a ) (dobra b)

possui :: Arvore -> Int -> Bool
possui Folha x = False
possui (Galho c a b) val = if( c== val ) then True else (possui a val || possui b val)

possui2 :: Arvore -> Int -> Bool
possui2 Folha x = False
possui2 (Galho c a b) val = if(c== val ) then True else (if val < c then possui2 a val else possui2 b val)

maximo :: Arvore -> Int
maximo Folha = 0
maximo (Galho val a b) = max val (maximo b)

insere :: Arvore -> Int -> Arvore
insere Folha val = (Galho val Folha Folha)
insere (Galho x a b) val = if(val < x) then (Galho x (insere a val) b) else (Galho x a (insere b val))
main = print ( dobra a2)