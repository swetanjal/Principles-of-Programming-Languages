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
searchEnv _ x = error "Variable not declared"

getMap :: Env -> Binding -> (ID, Value)
getMap e (Bind id a) = (id, (eval e a))

extendenv :: [Binding] -> Env -> Env
extendenv (el:ls) e = (getMap e el) : (extendenv ls e)
extendenv _ e = e

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


-- Support for user defined Procedures
eval env (Function ls a) = (Proc ls a env)
eval env (App ((Reference proc_name) : params)) = (eval (extendenv (createBinds (getFormalParamList proc_name env) params) (getClosureEnv proc_name env)) (getClosureAST proc_name env))
---------------------------------------------------------------------------------------------

-------------------------------- Inbuilt functions -------------------------------------------
add2 :: Value -> Value -> Int
add2 (NumVal a) (NumVal b) = (a + b)

mult2 :: Value -> Value -> Int
mult2 (NumVal a) (NumVal b) = (a * b)

sub2 :: Value -> Value -> Int
sub2 (NumVal a) (NumVal b) = (a - b)

isZero :: Value -> Bool
isZero (NumVal z) = (if z == 0
                        then True
                        else False)