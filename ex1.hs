xs:: [Int]
xs=[1,2,3,4]

par:: Int-> Bool
par n = n `mod` 2 == 0
pares::[Int]
pares=filter par xs
incrementa::Int->Int
incrementa n = n + 1
incs::[Int]
incs=map incrementa xs
dups:: Int -> Int
dups x = x*2
duplicado::[Int]
duplicado= map dups xs
maior:: Int
greatest:: Int->Int->Int
greatest x y= if x> y then x else y
maior=foldr greatest 0 xs
menor::Int
menorzin:: Int->Int->Int
menorzin x y= if x< y then x else y
menor=foldr menorzin maior xs

main=print(menor )
