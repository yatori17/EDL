bandas :: [[String]]
bandas = [ ["Gilberto Gil"],
           ["Victor","Leo"],
           ["Gonzagao"],
           ["Claudinho","Bochecha"] ]

musicas :: [(String, Int, Int)]
musicas = [ ("Aquele Abraco",       1, 100),
            ("Esperando na Janela", 1, 150),
            ("Borboletas",          2, 120),
            ("Asa Branca",          3, 120),
            ("Assum Preto",         3, 140),
            ("Vem Morena",          3, 200),
            ("Nosso Sonho",         4, 150),
            ("Quero te Encontrar",  4, 100) 
            ]
---3.1
nomeM:: (String, Int,Int)-> String
nomeM (x,_,_)=x
nomedasMusicas:: [String]
nomedasMusicas= map nomeM musicas
---3.2
durM2::(String,Int,Int)-> Bool
durM2 (_,_,x)= x>=120
tuplaMusicaM::[(String, Int, Int)]
tuplaMusicaM= filter durM2 musicas
--3.3
maior:: Int->Int->Int
maior x y= if x> y then x else y
dur::(String,Int,Int)-> Int
dur (_,_,x)=x
maiorD:: Int
maiorD= foldr maior 0 (map dur musicas)
----3.4
nomesM2::[String]
nomesM2=map nomeM tuplaMusicaM
---3.5
autor::(String,Int,Int)->[String]
autor (_,x,_)= (bandas!!x)
autores::String->String->String
autores a b=a ++ ", "++b
notTopretty::(String,Int,Int)->String
notTopretty (x,y,z)="nome "++ x ++"\nautores: "++ "\nDuracao: "++show z++"\n"
--aux::(String,Int,Int)-> String


main= print(foldr autores "" (bandas!!1))
