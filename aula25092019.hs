data Lista a= No a (Lista a) | Vazia
  deriving Show

data Arvore a= Galho a (Arvore a) (Arvore a) | Folha
  deriving Show



a1 = Galho 5( Galho 3 Folha (Galho 2 Folha ( Galho 1 Folha Folha))) (Galho 4 Folha Folha)
a2 = Galho 1 (Galho 3 Folha Folha) (Galho 7 Folha Folha)
quantasFolhas:: Arvore a-> Int
quantasFolhas Folha   = 1
quantasFolhas (Galho _ a1 a2)= quantasFolhas a1 + quantasFolhas a2

alturaArvore :: Arvore a-> Int
alturaArvore Folha = 1
alturaArvore ( Galho _ a1 a2)= 1+ maior( alturaArvore a1) (alturaArvore a2)
  where
    maior:: Int -> Int -> Int
    maior x y= if x> y then x else y

main= print ((quantasFolhas a1),(quantasFolhas a2),(alturaArvore a1), (alturaArvore a2))
