data Cmd = Atr String Exp
        |  Seq Cmd Cmd
        |  Dcl String
        deriving Show 
data Exp = Num Int
        |   Add Exp Exp 
        |   Sub Exp Exp
        |   Var String
        deriving Show
c0= Seq (Dcl "x") ( Atr "x" (Num 1))
c1 = Atr "x" (Num 10)
c2= Dcl "x"

verificaExp:: [String] -> Exp ->Bool
verificaExp [] (exp) = False
verificaExp arr (Var id')= elem id' arr
verificaExp arr (Add a b) = verificaExp arr a && verificaExp arr b
verificaExp arr (Sub a b) = verificaExp arr a && verificaExp arr b
verificaExp _ _ = True

verificaCmd:: [String] -> Cmd -> ([String], Bool)
verificaCmd arr (Atr id exp) = (arr, (elem id arr) && (verificaExp arr exp))
verificaCmd arr (Dcl id) = (id:arr, True)
verificaCmd arr (Seq a b ) = (v2, b1 && b2) where
            (v1,b1)= verificaCmd arr a
            (v2,b2)= verificaCmd v1 b

--verificaProg :: Cmd -> Bool




main = print ( verificaCmd [] c2)
