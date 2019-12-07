{-
Considere a seguinte representação para árvores binárias:

data Arvore = Folha | Galho Arvore Arvore

    Desenhe as árvores correspondentes aos seguintes valores:
        a1 = Galho (Galho Folha Folha) (Galho Folha Folha)
        a2 = Galho (Galho (Galho Folha Folha) Folha) Folha
        a3 = Galho Folha (Galho (Galho Folha Folha) (Galho Folha Folha))

    Defina a função folhas que recebe uma árvore e retorna a sua quantidade de folhas.

    Defina a função altura que recebe uma árvore e retorna a sua altura, ou seja, a maior distância entre a raíz e as folhas. (Uma árvore com apenas uma folha tem altura 0).

    Defina a função espelho que recebe uma árvore e inverte as subárvores de todos os galhos (troca a esquerda com a direita).

-}
data Arvore = Folha | Galho Arvore Arvore
    deriving Show
a1 = Galho (Galho Folha Folha) (Galho Folha Folha)
a2 = Galho (Galho (Galho Folha Folha) Folha) Folha
a3 = Galho Folha (Galho (Galho Folha Folha) (Galho Folha Folha))

folhas :: Arvore -> Int
folhas (Folha)= 1
folhas (Galho a b)= folhas a + folhas b

altura :: Arvore -> Int
altura Folha = 0
altura (Galho a b) = 1 + maior (altura a) (altura b) where 
    maior a b= if a> b then a else b
espelho :: Arvore -> Arvore
espelho Folha = Folha
espelho (Galho a b) = Galho (espelho b) (espelho a)

main = print ( espelho a2)