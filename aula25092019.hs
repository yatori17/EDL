data Lista a= No a (Lista a) | Vazia
  deriving Show

data Arvore a= Galho a (Arvore a) (Arvore a) | Folha
  deriving Show


a0 = Galho 10 Folha Folha
a1 = Galho 5( Galho 3 Folha (Galho 2 Folha ( Galho 1 Folha a0))) a0
a2 = Galho 1 a0 a0
quantasFolhas:: Arvore a-> Int
quantasFolhas Folha   = 1
quantasFolhas (Galho _ a1 a2)= quantasFolhas a1 + quantasFolhas a2

alturaArvore :: Arvore a-> Int
alturaArvore Folha = 1
alturaArvore ( Galho _ a1 a2)= 1+ maior( alturaArvore a1) (alturaArvore a2)
  where
    maior:: Int -> Int -> Int
    maior x y= if x> y then x else y

somaGalhos :: Arvore Int-> Int
somaGalhos Folha=0
somaGalhos (Galho v a1 a2)= v+ somaGalhos a1+ somaGalhos a2

main= print ((quantasFolhas a1),(quantasFolhas a2),(alturaArvore a1), (alturaArvore a2),somaGalhos a1,somaGalhos a2)
