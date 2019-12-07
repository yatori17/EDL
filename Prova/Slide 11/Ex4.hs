{-
    Baseado no exercício 2, crie uma nova representação que guarde nos galhos qualquer tipo de dados, ou seja, não necessariamente inteiros.

    Redefina todas as funções do exercício 2 para funcionarem com essa nova representação.
        Quais funções deixaram de funcionar? Por quê?
        Porque Haskell não consegue identificar o tipo e tenta fazer uma operação de soma com esses
        Adicione os prefixos Num a => ou Ord a => (ou ambos) nos exercícios que não funcionaram e verifique se é possível fazer com que todos funcionem.

    Defina uma função mapA com o seguinte tipo:
        mapA :: (a -> b) -> Arvore a -> Arvore b
        A função deve mapear todos os valores de uma árvore para uma nova árvore, usando uma função de mapeamento.
        Crie pelo menos 3 exemplos interessantes de uso dessa função.

    Defina uma função foldA com o seguinte tipo:
        foldA :: (a -> b -> b) -> b -> Arvore a -> b
        A função deve fazer um fold de todos os valores guardados na árvore recebida com a função passada e a partir de um acumulador inicial também recebido.
        Crie pelo menos 3 exemplos interessantes de uso dessa função.

-}

data Arvore  a = Folha | Galho a (Arvore a) (Arvore a)
    deriving Show
a1 = Galho 3 (Galho 2 Folha Folha) (Galho 3 Folha Folha)
a2 = Galho 1.5 (Galho 1 (Galho 2.5 Folha Folha) Folha) Folha
a3 = Galho 0 Folha (Galho 0 (Galho 0 Folha Folha) (Galho 0Folha Folha))

folhas :: Arvore a-> Int
folhas (Folha)= 1
folhas (Galho _ a b)= folhas a + folhas b

altura :: Arvore a-> Int
altura Folha = 0
altura (Galho _ a b) = 1 + maior (altura a) (altura b) where 
    maior a b= if a> b then a else b
espelho :: Arvore a-> Arvore a
espelho Folha = Folha
espelho (Galho c a b) = Galho c (espelho b) (espelho a)

soma :: Num a=> Arvore a-> a
soma Folha = 0
soma (Galho valor a b)= valor + (soma a) + (soma b) 

dobra :: Num a => Arvore a-> Arvore a
dobra Folha = Folha
dobra (Galho valor a b ) = Galho (valor*2) (dobra a ) (dobra b)

possui :: Ord a => Arvore a-> a -> Bool
possui Folha x = False
possui (Galho c a b) val = if( c== val ) then True else (possui a val || possui b val)

possui2 :: Ord a=>Arvore a-> a -> Bool
possui2 Folha x = False
possui2 (Galho c a b) val = if(c== val ) then True else (if val < c then possui2 a val else possui2 b val)

maximo ::(Ord a,Num a)=> Arvore a-> a
maximo Folha = 0
maximo (Galho val a b) = max val (maximo b)

insere :: (Ord a, Num a)=>Arvore a-> a -> Arvore a
insere Folha val = (Galho val Folha Folha)
insere (Galho x a b) val = if(val < x) then (Galho x (insere a val) b) else (Galho x a (insere b val))

mapA:: (a->b) -> Arvore a -> Arvore b
mapA f Folha = Folha
mapA f (Galho x a b)=(Galho (f x) (mapA f a) (mapA f b))

cubo:: Int -> Int
cubo x = x * x * x

todosPares:: Int -> Int
todosPares x= if((x `div` 2)== 0) then x else 2* x

foldA :: (a -> b -> b) -> b -> Arvore a -> b
foldA f init Folha = init
foldA f init (Galho x a b)= f x (foldA f (foldA f init  b) a)

pegaMaior:: Int -> Int -> Int
pegaMaior a b= if a > b then a else b


main = print ( foldA pegaMaior 0 a1)