import System.IO
import Control.Monad
import ASTParser

------------------------------ Defining Datatypes ---------------------------------------
data Value = NumVal Int | BoolVal Bool | Proc [ID] AST Env
            deriving Show
type Env = [(ID, Value)]
------------------------------------------------------------------------------------------

------------------------------ Helper Functions ------------------------------------------
getValInt :: Value -> Int
getValInt (NumVal d) = d
getValInt (BoolVal True) = error "Expected Int got Bool!" 
getValInt (BoolVal False) = error "Expected Int got Bool"

getValBool :: Value -> Bool
getValBool (BoolVal d) = d
getValBool (NumVal 0) = error "Expected Bool got Int"
getValBool (NumVal i) = error "Expected Bool got Int"
-------------------------------------------------------------------------------------------

--------- Takes an environment and a list of ASTs and evaluates them in the env -----------
evalASTList :: Env -> [AST] -> [Value]
evalASTList env (x:y) = (eval env x) : (evalASTList env y)
evalASTList env _ = []
-------------------------------------------------------------------------------------------

-------------- Functions to look up environment and extend environment --------------------
searchEnv :: Env -> ID -> Value
searchEnv (e:e_dash) x = (if (fst e) == x
                                then (snd e)
                                else (searchEnv e_dash x) )
searchEnv _ x = (error ("Variable not declared " ++ show x))

getMap :: Env -> Binding -> (ID, Value)
getMap e (Bind id a) = (id, (eval e a))

extendenv :: [Binding] -> Env -> Env
extendenv (el:ls) e = (getMap e el) : (extendenv ls e)
extendenv _ e = e

concatEnv :: Env -> Env -> Env
concatEnv (a : a_ls) b = a : (concatEnv a_ls b)
concatEnv _ (b : b_ls) = b : (concatEnv [] b_ls)
concatEnv _ _ = []
--------------------------------------------------------------------------------------------

getFormalParams :: Value -> [ID]
getFormalParams (Proc ls a e) = ls

getFormalParamList :: ID -> Env -> [ID]
getFormalParamList x (e:e_dash) = (if (fst e) == x
                                        then (getFormalParams (snd e))
                                        else (getFormalParamList x e_dash))

createBinds :: [ID] -> [AST] -> [Binding]
createBinds (x : id_ls) (y : ast_ls) = (Bind x y) : (createBinds id_ls ast_ls)
createBinds _ _ = []

procClosureEnv :: Value -> Env
procClosureEnv (Proc ls a e) = e

getClosureEnv :: ID -> Env -> Env
getClosureEnv x (e:e_dash) = (if (fst e) == x
                                then (procClosureEnv (snd e))
                                else (getClosureEnv x e_dash))

procClosureAST :: Value -> AST
procClosureAST (Proc ls a e) = a

getClosureAST :: ID -> Env -> AST
getClosureAST x (e:e_dash) = (if (fst e) == x
                                then (procClosureAST (snd e))
                                else (getClosureAST x e_dash))

--------------------------------------------------------------------------------------------
-- Helper Functions Handling Recursive Procedures

procFBind :: FBind -> Env -> (ID, Value)
procFBind (FBind id formals body) env = (id , (Proc formals body ((id, (Proc formals body env)) : env)))

getRecFunEnv :: [FBind] -> Env -> [(ID, Value)]
getRecFunEnv (e : e_dash) env = (procFBind e env) : (getRecFunEnv e_dash env)
getRecFunEnv _ env = []

extendenvByValue :: [(ID, Value)] -> Env -> Env
extendenvByValue (e : e_dash) env = e : (extendenvByValue e_dash env)
extendenvByValue _ env = env

------------------------------ Evaluator Function ------------------------------------------
eval :: Env -> AST -> Value
eval env (Number n) = NumVal n
eval env (Boolean b) = BoolVal b
eval env (If p1 p2 p3) = (if (getValBool (eval env p1)) == True
                            then (eval env p2)
                            else (eval env p3))
eval env (Reference v) = (searchEnv env v)
eval env (Assume ls tr) = (eval (extendenv ls env) tr)

-- ASTs for inbuilt functions
eval env (App [Reference "+", a, b]) = (NumVal (add2 (eval env a) (eval env b)))
eval env (App [Reference "-", a, b]) = (NumVal (sub2 (eval env a) (eval env b)))
eval env (App [Reference "*", a, b]) = (NumVal (mult2 (eval env a) (eval env b)))
eval env (App [Reference "/", a, b]) = (NumVal (div2 (eval env a) (eval env b)))
eval env (App [Reference "=", a, b]) = (BoolVal (equals (eval env a) (eval env b)))

eval env (App [Reference "or", a, b]) = (BoolVal (or2 (eval env a) (eval env b)))
eval env (App [Reference "and", a, b]) = (BoolVal (and2 (eval env a) (eval env b)))
eval env (App [Reference "not", a]) = (BoolVal (not1 (eval env a) ))
eval env (App [Reference "isZero", a]) = (BoolVal (isZero (eval env a)))

-- Support for user defined Procedures
eval env (Function ls a) = (Proc ls a env)
eval env (App ((Reference proc_name) : params)) = (eval (concatEnv (extendenv (createBinds (getFormalParamList proc_name env) params) env) (getClosureEnv proc_name env)) (getClosureAST proc_name env))

-- Support for recursive functions
eval env (RecFun ls tr) = (eval (extendenvByValue (getRecFunEnv ls env) env) tr)
---------------------------------------------------------------------------------------------

-------------------------------- Inbuilt functions -------------------------------------------
add2 :: Value -> Value -> Int
add2 (NumVal a) (NumVal b) = (a + b)
add2 _ _ = error "Types can't be matched!"


mult2 :: Value -> Value -> Int
mult2 (NumVal a) (NumVal b) = (a * b)
mult2 _ _ = error "Types can't be matched!"


sub2 :: Value -> Value -> Int
sub2 (NumVal a) (NumVal b) = (a - b)
sub2 _ _ = error "Types can't be matched!"

div2 :: Value -> Value -> Int
div2 (NumVal a) (NumVal 0) = error "Division by zero!"
div2 (NumVal a) (NumVal b) = (div a b)
div2 _ _ = error "Types can't be matched!"

isZero :: Value -> Bool
isZero (NumVal z) = (if z == 0
                        then True
                        else False)
isZero _ = error "Types can't be matched!"

equals :: Value -> Value -> Bool
equals (NumVal a) (NumVal b) = (if a == b
                                    then True
                                    else False)
equals _ _ = error "Types can't be matched!"

or2 :: Value -> Value -> Bool
or2 (BoolVal a) (BoolVal b) = (if a == True || b == True
                                then True
                                else False)
or2 _ _ = error "Types can't be matched!"

and2 :: Value -> Value -> Bool
and2 (BoolVal a) (BoolVal b) = (if a == True && b == True
                                    then True
                                    else False)
and2 _ _ = error "Types can't be matched!"


not1 :: Value -> Bool
not1 (BoolVal a) = (if a == True
                        then False
                        else True)
not1 _ = error "Types can't be matched!"
------------------------------- Main Function -------------------------------------------------
run :: String -> Value
run program = (eval [] (parseString program))

------------------------------------- Examples ------------------------------------------------
-- run "(recfun ((iseven (x) (if (= x 0) True (isodd (- x 1)))) .  (isodd (x) (if (= x 0) False (iseven (- x 1)) )) ) (isodd 12))"
-- run "(recfun ((factorial (x) (if (= x 0) 1 (* x (factorial2 (- x 1))))) .  (factorial2 (x) (if (= x 0) 1 (* x (factorial (- x 1))))) ) (factorial2 10))"
-- run "(recfun ((factorial (x) (if (= x 0) 1 (* x (factorial (- x 1)))))) (factorial 6))"