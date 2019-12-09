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
         | App2 String (Exp,Exp)
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
avaliaExp (mem, cod) (App2 id (e1,e2)) = ret where 
                                    ret = consulta mem1 "ret"
                                    (mem1,_) = avaliaCmd (mem'',cod) fun2    
                                    mem'' = escreve mem' "arg" arg
                                    arg = avaliaExp (mem,cod) e1
                                    mem'= escreve mem "arg2" arg2 
                                    arg2 = avaliaExp (mem,cod) e2 
                                    fun2 = consulta cod id

-------------------------------------------------------------------------------

type Cod = [(String,Cmd)]
type Env = (Mem,Cod)

data Cmd = Atr String Exp
         | Prt Exp
         | Seq Cmd Cmd
         | Cnd Exp Cmd Cmd
         | Fun String String Cmd
         | Fun2 String (String,String) Cmd
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
avaliaCmd (mem,cod) (Fun id indi c)      = (mem, escreve cod id cmd) where
                                        cmd = Seq (Atr indi (Var "arg")) (c)
avaliaCmd (mem,cod) (Fun2 id ( x , y ) c) = (mem, escreve cod id c') where
                                        c' = Seq (Atr x (Var "arg")) (Seq (Atr y (Var "arg2")) c)


p2 = Seq
      (Fun "soma" "x"
        (Cnd (Var "x")
          (Atr "ret"
            (Add (Var "x")
                 (App "soma"
                   (Sub (Var "x")
                        (Num 1)))))
          (Atr "ret" (Num 0))))
      (Prt (App "soma" (Num 10)))
{-
y = 10
def f (x):
    y = x
    print(y)    # ???
    return y

print(f(20))
print(y)    
-}

p3 = Seq (Atr "y" (Num 10)) 
    (Seq
        (Fun "f" "x" 
            (Seq 
                (Atr "y" (Var "x")) 
                (Seq 
                    (Prt (Var "y")) 
                    (Atr "ret" (Var "y"))))) 
        (Seq 
            (Prt (App "f" (Num 20)))
            (Prt (Var "y"))))
p4 = Seq 
        (Fun2 "g" ("a","b")
            (Atr "ret" 
                (Add (Var "a") (Var "b")))) 
        
        (Prt (App2 "g" ((Num 10),(Num 5)))) 

main = print (avaliaCmd ([],[]) p4)