turma1 :: [ (String,Float,Float) ]
turma1 = [ ("Joao",7.5,3.5), ("Maria",10.0,8.0), ("Jose",5.0,3.0),("Eduardo", 7.0,8.0)]
nome :: (String,Float,Float) -> String
nome (nm,_,_) = nm

nota1 :: (String,Float,Float) -> Float
nota1 (_,n1,_) = n1

nota2 :: (String,Float,Float) -> Float
nota2 (_,_,n2) = n2
--- Retorna media---
media :: (String,Float,Float) -> Float
media aluno = ((nota1 aluno) + (nota2 aluno)) / 2
--mdaturma:: [Float]
--mdaturma= map media turma1
-- medias func. You need to use func <Array of tuples>
medias:: [ (String, Float, Float)] -> [Float]
medias turma= map media turma1

---NOTAS 1--
notas1 :: [Float]
notas1= map nota1 turma1

---NOTAS 2--
notas2 :: [Float]
notas2= map nota2 turma1

--- Nota > 8
notaM8 :: (String, Float, Float) -> Bool
notaM8 aluno = (nota1 aluno>=8) && (nota2 aluno >=8)

-- 8s func,ex: oitos turma1 
oitos :: [ (String, Float, Float)] -> [String]
oitos turma= map nome( filter notaM8 turma1)

mediaF:: Float->Float-> Float
mediaF a b= (a+b)/2
todas :: [ (String,Float,Float) ] -> (Float, Float, Float)
todas turma = (foldr mediaF 0 notas1, foldr mediaF 0 notas2, foldr mediaF 0 (medias turma1))

pegami5:: Float-> Bool
pegami5 x= x<=5

menorq5:: [(String, Float, Float)] -> [Float]
menorq5 turma = filter pegami5 (notas1 ++ notas2)
baixas :: [Float]
baixas= menorq5 turma1

--Imprimir !
--Converter Medias em String


situacao:: (String, Float,Float)-> String
situacao aluno= if(((nota1 aluno)+(nota2 aluno))/2 >5)then "(aprovado)" else  "(reprovado)"
aSolution:: (String,Float,Float)-> String
aSolution aluno= nome aluno ++" "++ show(((nota1 aluno)+(nota2 aluno))/2)++ " " ++(situacao aluno)
keepgoing:: [(String, Float, Float)] -> [String]
keepgoing turma = map aSolution turma1
nottopretty:: String->String -> String
nottopretty str aluno= str ++"\n"++aluno
pretty :: [ (String,Float,Float) ] -> String
pretty turma=foldr nottopretty "" (keepgoing turma1)

main = putStrLn (pretty turma1)
