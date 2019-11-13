import System.IO
import Control.Monad
import ASTParser

------------------------------ Defining Datatypes ---------------------------------------
data Value = NumVal Int | BoolVal Bool | Proc [ID] AST Env
            deriving Show
type Env = [(ID, Value)]
type Store = [Value]
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

------------------------------ Helper Functions for Stores ---------------------------------

getIndex :: Value -> Int
getIndex (NumVal i) = i


storeExtend :: Store -> Value -> (Value, Store)
storeExtend s v = (NumVal (length s), s ++ [v])

storeGet :: Store -> Value -> Value
storeGet s (NumVal i) = s !! i

getNewStore :: Store -> Value -> Value -> Int -> Store
getNewStore (x : rem) idx v len = if (len - (length rem)) == (getIndex idx)
                                            then v : (getNewStore rem idx v len)
                                            else x : (getNewStore rem idx v len)
getNewStore _ _ _ _ = []

storeSet :: Store -> Value -> Value -> (Value, Store)
storeSet s i v = (NumVal 0, (getNewStore s i v (length s)))

-------------------------------------------------------------------------------------------

--------- Takes an environment and a list of ASTs and evaluates them in the env -----------
--evalASTList :: Env -> Store -> [AST] -> [Value]
--evalASTList env s (x:y) = (eval env s x) : (evalASTList env s y)
--evalASTList env _ _ = []
-------------------------------------------------------------------------------------------

-------------- Functions to look up environment and extend environment --------------------
searchEnv :: Env -> ID -> Value
searchEnv (e:e_dash) x = (if (fst e) == x
                                then (snd e)
                                else (searchEnv e_dash x) )
searchEnv _ x = (error ("Variable not declared " ++ show x))

getMap :: Env -> Store -> Binding -> ((ID, Value), Store)
getMap e s (Bind id a) = ((id, (fst (eval e s a))), (snd (eval e s a))) 

extendenv :: [Binding] -> Env -> Store -> (Env, Store)
extendenv (el:ls) e s = ((fst (getMap e s el)) : (fst (extendenv ls e (snd (getMap e s el)))), (snd (extendenv ls e (snd (getMap e s el)))))
extendenv _ e s = (e, s)

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
eval :: Env -> Store -> AST -> (Value, Store)
eval env s (Number n) = (NumVal n, s)
eval env s (Boolean b) = (BoolVal b, s)
eval env s (If p1 p2 p3) = (if (getValBool (fst (eval env s p1)) ) == True
                            then (eval env (snd (eval env s p1)) p2)
                            else (eval env (snd (eval env s p1)) p3))
eval env s (Reference v) = ((searchEnv env v), s)
eval env s (Assume ls tr) = (eval (fst (extendenv ls env s)) (snd (extendenv ls env s)) tr)

-- ASTs for inbuilt functions
eval env s (App [Reference "+", a, b]) = ((NumVal (add2 (fst (eval env s a)) (fst (eval env (snd (eval env s a)) b)) )), (snd (eval env (snd (eval env s a)) b)))
eval env s (App [Reference "-", a, b]) = ((NumVal (sub2 (fst (eval env s a)) (fst (eval env (snd (eval env s a)) b)) )), (snd (eval env (snd (eval env s a)) b)))
eval env s (App [Reference "*", a, b]) = ((NumVal (mult2 (fst (eval env s a)) (fst (eval env (snd (eval env s a)) b)) )), (snd (eval env (snd (eval env s a)) b)))
eval env s (App [Reference "/", a, b]) = ((NumVal (div2 (fst (eval env s a)) (fst (eval env (snd (eval env s a)) b)) )), (snd (eval env (snd (eval env s a)) b)))

eval env s (App [Reference "=", a, b]) = ((BoolVal (equals (fst (eval env s a)) (fst (eval env (snd (eval env s a)) b)) )), (snd (eval env (snd (eval env s a)) b)))

eval env s (App [Reference "or", a, b]) = ((BoolVal (or2 (fst (eval env s a)) (fst (eval env (snd (eval env s a)) b)))), (snd (eval env (snd (eval env s a)) b)))
eval env s (App [Reference "and", a, b]) = ((BoolVal (and2 (fst (eval env s a)) (fst (eval env (snd (eval env s a)) b)))), (snd (eval env (snd (eval env s a)) b)))
eval env s (App [Reference "not", a]) = ((BoolVal (not1 (fst (eval env s a)))), (snd (eval env s a)))
eval env s (App [Reference "isZero", a]) = ((BoolVal (isZero (fst (eval env s a)))), (snd (eval env s a)))


-- Support for user defined Procedures
eval env s (Function ls a) = ((Proc ls a env), s)
eval env s (App ((Reference proc_name) : params)) = (eval (concatEnv (fst (extendenv (createBinds (getFormalParamList proc_name env) params) env s)) (getClosureEnv proc_name env)) (snd (extendenv (createBinds (getFormalParamList proc_name env) params) env s)) (getClosureAST proc_name env))

-- Support for recursive functions
eval env s (RecFun ls tr) = (eval (extendenvByValue (getRecFunEnv ls env) env) s tr)

-- Support for Stores
eval env s (NewRef a) = (storeExtend s (fst (eval env s a)))
eval env s (SetRef idx a) = (storeSet s (fst (eval env s idx)) (fst (eval env s a)))
eval env s (DeRef idx) = ((storeGet s (fst (eval env s idx))), s)



eval _ _ _ = error "Invalid Syntax!"
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
run program = (fst (eval [] [] (parseString program)))

------------------------------------- Examples ------------------------------------------------
-- run "(recfun ((iseven (x) (if (= x 0) True (isodd (- x 1)))) .  (isodd (x) (if (= x 0) False (iseven (- x 1)) )) ) (isodd 12))"
-- run "(recfun ((factorial (x) (if (= x 0) 1 (* x (factorial2 (- x 1))))) .  (factorial2 (x) (if (= x 0) 1 (* x (factorial (- x 1))))) ) (factorial2 10))"
-- run "(recfun ((factorial (x) (if (= x 0) 1 (* x (factorial (- x 1)))))) (factorial 6))"
-- run "(assume ((r (newref 90))) (deref r))"
