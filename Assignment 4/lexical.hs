import ASTParser

-- Defining Expressible/Denotable Values
data Value = NumVal Int | BoolVal Bool

-- Defining the structure AST
data AST = Number Int | Boolean Bool | PrimApp Op [AST] | If AST AST AST | Assume [Bind] AST | Variable Id

-- Defining the structure Env
type Env = [(Id, Value)]

type Id = String

data Bind = Bind Id AST

-- Helper Functions
getValInt :: Value -> Int
getValInt (NumVal d) = d
getValInt (BoolVal True) = error "Expected Int got Bool!" 
getValInt (BoolVal False) = error "Expected Int got Bool"

getValBool :: Value -> Bool
getValBool (BoolVal d) = d
getValBool (NumVal 0) = error "Expected Bool got Int"
getValBool (NumVal i) = error "Expected Bool got Int"

-- Primary Operators
evalValueList :: Op -> [Value] -> Value
evalValueList Add (x:ls) = (NumVal ((getValInt x) + (getValInt (evalValueList Add ls))))
evalValueList Add _ = (NumVal 0)
evalValueList Sub (x:ls) = (NumVal ((getValInt x) - (getValInt (evalValueList Sub ls))))
evalValueList Sub _ = (NumVal 0)
evalValueList Mul (x:ls) = (NumVal ((getValInt x) * (getValInt (evalValueList Mul ls))))
evalValueList Mul _ = (NumVal 1)
evalValueList Div (x:ls) = (NumVal (div (getValInt x) (getValInt (evalValueList Div ls))))
evalValueList Div _ = (NumVal 1)
evalValueList And (x:ls) = (BoolVal ((getValBool x) && (getValBool (evalValueList And ls))))
evalValueList And _ = (BoolVal True)
evalValueList Or (x:ls) = (BoolVal ((getValBool x) || (getValBool (evalValueList Or ls))))
evalValueList Or _ = (BoolVal False)
evalValueList Not (x:ls) = (BoolVal (not (getValBool x)))
evalValueList IsZero (x:ls) = (if (getValInt x) == 0
                                then (BoolVal True)
                                else (BoolVal False))

-- Helper Functions
evalASTList :: Env -> [AST] -> [Value]
evalASTList env (x:y) = (eval env x) : (evalASTList env y)
evalASTList env _ = []

-- Functions to look up environment and extend environment
searchEnv :: Env -> Id -> Value
searchEnv (e:e_dash) x = (if (fst e) == x
                                then (snd e)
                                else (searchEnv e_dash x) )
searchEnv _ x = error "Variable not declared"

getMap :: Env -> Bind -> (Id, Value)
getMap e (Bind id a) = (id, (eval e a))

extendenv :: [Bind] -> Env -> Env
extendenv (el:ls) e = (getMap e el) : (extendenv ls e)
extendenv _ e = e

-- Evaluator
eval :: Env -> AST -> Value
eval env (Number n) = NumVal n
eval env (Boolean b) = BoolVal b
eval env (PrimApp op args) = (evalValueList op (evalASTList env args))
eval env (If p1 p2 p3) = (if (getValBool (eval env p1)) == True
                            then (eval env p2)
                            else (eval env p3))
eval env (Variable v) = (searchEnv env v)
eval env (Assume ls tr) = (eval (extendenv ls env) tr)

-- Takes a list of ParseTree (BindAppl type) and returns a list of Bind
getBinds :: [ParseTree] -> [Bind]
getBinds [BindAppl (Symbol x) z] = [(Bind x (toAST z))]
getBinds ((BindAppl (Symbol x) z) : y) = (Bind x (toAST z)) : (getBinds y)


-- Convert Parse Tree to Abstract Syntax Tree
toAST :: ParseTree -> AST
toAST (Numeric n) = Number n
toAST (Booleric b) = Boolean b
toAST (Symbol x) = Variable x
toAST (Appl op ls) = PrimApp op (map toAST ls)
toAST (IfAppl p1 p2 p3) = If (toAST p1) (toAST p2) (toAST p3)
toAST (AssumeAppl (BindSeq ls) p1) = Assume (getBinds ls) (toAST p1)
toAST _ = error "Parse Error"

-- Functions to print AST, Value, Bind ...

instance Show Bind where
    show (Bind id ast) = "(" ++ (show id) ++ " " ++ (show ast) ++ ")"

instance Show AST where
    show (Number x) = "(Number " ++ (show x) ++ ")"
    show (Variable x) = "(Variable " ++ (show x) ++ ")"
    show (Boolean b) = "(Boolean " ++ (show b) ++ ")"
    show (PrimApp op ls) = "(PrimApp " ++ (show op) ++ " " ++ (show (map show ls)) ++ ")"
    show (If a1 a2 a3) = "(If " ++ (show a1) ++ " " ++ (show a2) ++ " " ++ (show a3) ++ ")"
    show (Assume ls a1) = "(Assume " ++ (show (map show ls)) ++ " " ++ (show a1) ++  ")"

instance Show Value where
    show (NumVal i) = (show i)
    show (BoolVal b) = (show b)