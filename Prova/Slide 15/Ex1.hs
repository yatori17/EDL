-- exs
--  - pares
--  - multiplos args
--  - Exp.Arg
--  - Exp.Func
--  - data Mem a

import Debug.Trace

type Mem = [(String,Int)]

consulta :: [(String,a)] -> String -> a
consulta []           id = undefined
consulta ((id',v'):l) id = if id == id' then
                            v'
                           else
                            consulta l id

escreve :: [(String,a)] -> String -> a -> [(String,a)]
escreve l id v = (id,v):l

-------------------------------------------------------------------------------

data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Var String
         | App String Exp
  deriving Show

avaliaExp :: Env -> Exp -> Int
avaliaExp _         (Num v)     = v
avaliaExp env       (Add e1 e2) = (avaliaExp env e1) + (avaliaExp env e2)
avaliaExp env       (Sub e1 e2) = (avaliaExp env e1) - (avaliaExp env e2)
avaliaExp (mem,_)   (Var id)    = consulta mem id
avaliaExp (mem,cod) (App id e)  = ret where
                                    ret = consulta mem'' "ret"
                                    (mem'',_) = avaliaCmd (mem',cod) fun
                                    mem' = escreve mem "arg" arg
                                    arg  = avaliaExp (mem,cod) e
                                    fun  = consulta cod id

-------------------------------------------------------------------------------

type Cod = [(String,Cmd)]
type Env = (Mem,Cod)

data Cmd = Atr String Exp
         | Prt Exp
         | Seq Cmd Cmd
         | Cnd Exp Cmd Cmd
         | Fun String Cmd
  deriving Show

avaliaCmd :: Env -> Cmd -> Env
avaliaCmd env       (Prt e)         = traceShow (avaliaExp env e) env
avaliaCmd (mem,cod) (Atr id exp)    = (escreve mem id v,cod) where
                                        v = avaliaExp (mem,cod) exp
avaliaCmd env       (Seq c1 c2)     = avaliaCmd env' c2 where
                                        env' = avaliaCmd env c1
avaliaCmd env       (Cnd exp c1 c2) = if (avaliaExp env exp) /= 0 then
                                        avaliaCmd env c1
                                      else
                                        avaliaCmd env c2
avaliaCmd (mem,cod) (Fun id c)      = (mem, escreve cod id c)


p2 = Seq
      (Fun "soma"
        (Cnd (Var "arg")
          (Atr "ret"
            (Add (Var "arg")
                 (App "soma"
                   (Sub (Var "arg")
                        (Num 1)))))
          (Atr "ret" (Num 0))))
      (Prt (App "soma" (Num 10)))
{-
x = 1
y = 2
print(x + y)
-}
py1 = Seq (Prt (Add (Var "x") (Var "y"))) (Seq (Atr "x" (Num 1)) (Atr "y" (Num 2)))
{-def duplica (x):
return x+x
print(duplica(10))
-}
py2 = Seq 
        (Fun "duplica"
            (Atr "ret" 
              (Add (Var "arg")
              (Var "arg")))) 
        (Prt (App "duplica" (Num 15)))
{-
def soma (v):
    if v != 0:
        return v + soma(v-1)
    else:
        return 0
-}
py3 = Seq 
        (Fun "soma"
            (Cnd (Var "arg")
                (Atr "ret"
                    (Add (Var "arg")
                        (App "soma"
                            (Sub (Var "arg") 
                                (Num 1)))))
                (Atr "ret" (Num 0))))
        (Prt (App "soma" (Num 10)))

main = print (avaliaCmd ([],[]) py3)