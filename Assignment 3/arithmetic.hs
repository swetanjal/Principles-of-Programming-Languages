import Text.Read
import System.IO
import Control.Monad
-- Defining a Function to Check whether a String is an Int or not
isInt :: [Char] -> Bool
isInt s = case readMaybe s :: Maybe Int of
    Just _ -> True
    Nothing -> False

-- Defining a Function to Check whether a String is a Bool or not
isBool :: [Char] -> Bool
isBool s = case readMaybe s :: Maybe Bool of
    Just _ -> True
    Nothing -> False

-- Defining the Abstract Syntax Tree(AST) Datatype
data Tree = Leaf [Char] | Node [Char] Tree Tree

-- evalAST takes in an AST an answer/error as a list of characters
evalAST :: Tree -> [Char]
evalAST (Leaf x) = (if (isInt(x) == True || isBool(x) == True)
                        then x
                        else "Parse Error")
evalAST (Node op leftast rightast) = 
                                if op == "+"
                                    then (if (isInt (evalAST leftast) == True && (isInt (evalAST rightast)) == True)
                                            then show ((read (evalAST leftast) :: Int) + (read (evalAST rightast) :: Int))
                                            else (if ((evalAST leftast) == "Error: Division by Zero" || (evalAST rightast) == "Error: Division by Zero")
                                                                then "Error: Division by Zero"
                                                                else
                                                                "Parse Error"))
                                    else if op == "-"
                                        then (if (isInt (evalAST leftast) == True && (isInt (evalAST rightast)) == True)
                                                then show ((read (evalAST leftast) :: Int) - (read (evalAST rightast) :: Int))
                                                else (if ((evalAST leftast) == "Error: Division by Zero" || (evalAST rightast) == "Error: Division by Zero")
                                                                then "Error: Division by Zero"
                                                                else
                                                                "Parse Error"))
                                        else if op == "*"
                                            then (if (isInt (evalAST leftast) == True && (isInt (evalAST rightast)) == True)
                                                    then show ((read (evalAST leftast) :: Int) * (read (evalAST rightast) :: Int))
                                                    else (if ((evalAST leftast) == "Error: Division by Zero" || (evalAST rightast) == "Error: Division by Zero")
                                                                then "Error: Division by Zero"
                                                                else
                                                                "Parse Error"))
                                            else if op == "/"
                                                then (if (isInt(evalAST rightast) == True && (read (evalAST rightast) :: Int) == 0)
                                                        then "Error: Division by Zero"
                                                        else if (isInt (evalAST leftast) == True && (isInt (evalAST rightast)) == True)
                                                            then show (div (read (evalAST leftast) :: Int) (read (evalAST rightast) :: Int))
                                                            else (if ((evalAST leftast) == "Error: Division by Zero" || (evalAST rightast) == "Error: Division by Zero")
                                                                then "Error: Division by Zero"
                                                                else
                                                                "Parse Error"))
                                                else if op == "IsZero"
                                                    then (if (isInt(evalAST leftast) == True)
                                                            then (if ((read (evalAST leftast) :: Int) == 0)
                                                                    then "True"
                                                                    else "False")
                                                            else (if ((evalAST leftast) == "Error: Division by Zero" || (evalAST rightast) == "Error: Division by Zero")
                                                                then "Error: Division by Zero"
                                                                else
                                                                "Parse Error"))
                                                    else if op == "="
                                                        then (if (isInt(evalAST leftast) == True && isInt(evalAST rightast) == True)
                                                                then show ((read (evalAST leftast) :: Int) == (read (evalAST rightast) :: Int))
                                                                else if (isBool(evalAST leftast) == True && isBool(evalAST rightast) == True)
                                                                    then show ((read (evalAST leftast) :: Bool) == (read (evalAST rightast) :: Bool))
                                                                    else "Parse Error")
                                                        else (if ((evalAST leftast) == "Error: Division by Zero" || (evalAST rightast) == "Error: Division by Zero")
                                                                then "Error: Division by Zero"
                                                                else
                                                                "Parse Error")


-- parseSpace takes a String and matches one or more spaces and returns back remaining String
parseSpace :: [Char] -> [Char]
parseSpace (' ':s) = parseSpace s
parseSpace k = k

-- parseOpenBracket takes a String and matches exactly one opening Bracket else returns "Parse Error"
parseOpenBracket :: [Char] -> [Char]
parseOpenBracket ('(' : s) = s
parseOpenBracket _ = "Parse Error"

-- parseOperator takes a String and matches exactly one operator else returns ("Parse Error", "")
parseOperator :: [Char] -> ([Char], [Char])
parseOperator ('I' : 's' : 'Z' : 'e' : 'r': 'o' : ' ' : s) = ("IsZero", s)
parseOperator ('+': ' ' : s) = ("+" ,s)
parseOperator ('*' : ' ' : s) = ("*", s)
parseOperator ('-' : ' ' : s) = ("-", s)
parseOperator ('/' : ' ' : s) = ("/", s)
parseOperator ('=' : ' ' : s) = ("=", s)
parseOperator _ = ("Parse Error", "")

-- parseOperand takes a String and matches exactly one unsigned operand
parseOperand :: [Char] -> ([Char], [Char])
parseOperand ('0':s) = ('0': fst (parseOperand s) , snd(parseOperand s))
parseOperand ('1':s) = ('1': fst (parseOperand s) , snd(parseOperand s))
parseOperand ('2':s) = ('2': fst (parseOperand s) , snd(parseOperand s))
parseOperand ('3':s) = ('3': fst (parseOperand s) , snd(parseOperand s))
parseOperand ('4':s) = ('4': fst (parseOperand s) , snd(parseOperand s))
parseOperand ('5':s) = ('5': fst (parseOperand s) , snd(parseOperand s))
parseOperand ('6':s) = ('6': fst (parseOperand s) , snd(parseOperand s))
parseOperand ('7':s) = ('7': fst (parseOperand s) , snd(parseOperand s))
parseOperand ('8':s) = ('8': fst (parseOperand s) , snd(parseOperand s))
parseOperand ('9':s) = ('9': fst (parseOperand s) , snd(parseOperand s))
parseOperand ('T':'r':'u':'e':' ':s) = ("True", s)
parseOperand ('F':'a':'l':'s':'e':' ':s) = ("False", s)
parseOperand (' ' : k) = ("", k)
parseOperand (')' : k) = ("", k)
parseOperand (l:s) = (l: fst (parseOperand s) , snd(parseOperand s))
parseOperand _ = ("", "")

-- parseSignedOperand takes a String and matches exactly one signed operand
parseSignedOperand :: [Char] -> ([Char], [Char])
parseSignedOperand ('-' : s) = ('-' : fst (parseOperand s), snd (parseOperand s))
parseSignedOperand s = parseOperand s

-- parseSubExpression takes a String and returns a Subexpression and remaining expression
parseSubExpression :: [Char] -> Int -> ([Char], [Char])
parseSubExpression s 0 = ("", s)
parseSubExpression "" q = ("Parse Error", "Parse Error")
parseSubExpression ('(':s) k = ('(': fst(parseSubExpression s (k + 1)) , snd(parseSubExpression s (k + 1)))
parseSubExpression (')':s) k = (')': fst(parseSubExpression s (k - 1)) , snd(parseSubExpression s (k - 1)))
parseSubExpression (k':s) k = (k': fst(parseSubExpression s k) , snd(parseSubExpression s k))

getoperand2 :: [Char] -> [Char]
getoperand2 k = "Parse Error"

-- buildAST takes a String and creates an AST out of it
buildAST :: [Char] -> Tree
buildAST str = 
            if (snd (parseSubExpression (parseOpenBracket (parseSpace str)) 1)) == "Parse Error"
                then (Leaf "")
            else
            if op /= "Parse Error"
            then    
                if (isInt exp1) == True || (isBool exp1) == True
                    then
                    if (isInt exp2) == True || (isBool exp2) == True
                        then
                        (Node op (Leaf exp1) (Leaf exp2))
                        else
                        (Node op (Leaf exp1) (buildAST exp2))
                else
                    if (isInt exp2) == True || (isBool exp2) == True
                        then
                        (Node op (buildAST exp1) (Leaf exp2))
                        else
                        (Node op (buildAST exp1) (buildAST exp2))
            else
                (Leaf (fst(parseSignedOperand (parseSpace (parseOpenBracket (parseSpace str))))) )
            where 
                op = fst (parseOperator (parseSpace (parseOpenBracket (parseSpace str))))
                exp1 = if (parseOpenBracket(( parseSpace(snd (parseOperator (parseSpace (parseOpenBracket (parseSpace str)))))))) == "Parse Error"
                        then
                            fst (parseSignedOperand( parseSpace(snd (parseOperator (parseSpace (parseOpenBracket (parseSpace str)))))))
                        else
                            '(': (fst(parseSubExpression (parseOpenBracket(( parseSpace(snd (parseOperator (parseSpace (parseOpenBracket (parseSpace str)))))))) 1))
                exp2 = if (parseOpenBracket(( parseSpace(snd (parseOperator (parseSpace (parseOpenBracket (parseSpace str)))))))) == "Parse Error"
                        then
                            let sec_part = snd (parseSignedOperand( parseSpace(snd (parseOperator (parseSpace (parseOpenBracket (parseSpace str)))))))
                            in
                                if (parseOpenBracket(( parseSpace(sec_part)))) == "Parse Error"
                                    then
                                    fst (parseSignedOperand( parseSpace( sec_part)))
                                else '(': (fst (parseSubExpression (parseOpenBracket(( parseSpace(sec_part)))) 1))
                        else 
                            let sec_part = (snd(parseSubExpression (parseOpenBracket(( parseSpace(snd (parseOperator (parseSpace (parseOpenBracket (parseSpace str)))))))) 1))
                            in  if (parseOpenBracket(( parseSpace(sec_part)))) == "Parse Error"
                                    then
                                    fst (parseSignedOperand( parseSpace( sec_part)))
                                else '(': (fst (parseSubExpression (parseOpenBracket(( parseSpace(sec_part)))) 1))

-- REPL

read' :: IO String
read' =  putStr "Arithmetic-Lang> "
    >> hFlush stdout
    >> getLine

eval' :: String -> String
eval' input = evalAST ( buildAST input )

print' :: String -> IO ()
print' = putStrLn

main :: IO ()
main = do
  input <- read'
  
  unless (input == ":quit")
       $ print' (eval' input) >> main
