turma1 :: [ (String,Float,Float) ]
turma1 = [ ("Joao",7.5,3.5), ("Maria",10.0,8.0), ("Jose",5.0,3.0),("Eduardo", 7.0,8.0)]
nome :: (String,Float,Float) -> String
nome (nm,_,_) = nm

nota1 :: (String,Float,Float) -> Float
nota1 (_,n1,_) = n1

nota2 :: (String,Float,Float) -> Float
nota2 (_,_,n2) = n2

media :: (String,Float,Float) -> Float
media aluno = ((nota1 aluno) + (nota2 aluno)) / 2

medias :: [ (String,Float,Float) ] -> [Float]
medias turma = map media turma1

notas1 :: [Float]
notas1 = map nota1 turma1

notaboas:: (String,Float,Float)->Bool
notaboas (a,b,c)= 

oitos:: [(String,Float,Float)]->[String]
oitos turma1=filter notaboas turma1

main = print(oitos)
