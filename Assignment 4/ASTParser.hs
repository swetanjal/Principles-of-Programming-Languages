
module ASTParser where

import Data.String
import ParseUtils

parse :: String -> ParseTree
parse = (recurReduce 0) . (map toType) . tokenize
type Ident = String

data ParseTree =
    LBracket
  | RBracket                             
  | Operator Op                          
  | Numeric Int                          
  | Booleric Bool                        
  | Symbol Ident                         
  | IfKeyw                               
  | AssumeKeyw                           
  | BindAppl ParseTree ParseTree
  | BindSeq [ParseTree]
  | Appl Op [ParseTree]                  
  | IfAppl ParseTree ParseTree ParseTree 
  | AssumeAppl ParseTree ParseTree     
  deriving (Show)

isEqv :: ParseTree -> ParseTree -> Bool
isEqv LBracket LBracket = True
isEqv RBracket RBracket = True
isEqv (Numeric _) (Numeric _) = True
isEqv (Booleric _) (Booleric _) = True
isEqv (Operator _) (Operator _) = True
isEqv (Symbol _) (Symbol _) = True
isEqv IfKeyw IfKeyw = True
isEqv AssumeKeyw AssumeKeyw = True
isEqv (BindAppl _ _) (BindAppl _ _) = True
isEqv (BindSeq _) (BindSeq _) = True
isEqv (Appl _ _) (Appl _ _) = True
isEqv (IfAppl _ _ _) (IfAppl _ _ _) = True
isEqv (AssumeAppl _ _) (AssumeAppl _ _) = True
isEqv a b = False


isExpr :: ParseTree -> Bool
isExpr (Booleric _) = True
isExpr (Numeric _) = True
isExpr (Symbol _) = True
isExpr (BindAppl _ _) = True
isExpr (Appl _ _) = True
isExpr (IfAppl _ _ _) = True
isExpr (AssumeAppl _ _) = True
isExpr a = False

data Op = Add | Sub | Mul | Div | And | Or | Not | IsZero
          deriving (Eq)

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show And = "&"
    show Or = "|"
    show Not = "~"
    show IsZero = "zero?"


seqMatch :: [(ParseTree -> Bool)] -- predicate list
         -> [ParseTree]
         -> Bool
seqMatch preds seq =
  let
    applyPred f a = f a
    matches = zipWith applyPred preds seq
  in
    foldr (&&) True matches
          

toType :: String -> ParseTree
toType s 
  | isEq "(" s  = LBracket
  | isEq ")" s  = RBracket
  | isKey s      = toKey s
  | isNumber s  = Numeric (read s)
  | isBoolean s = Booleric (read s)
  | isAlNum s   = Symbol s
  | otherwise   = error "Incorrect syntax!"


toKey :: String -> ParseTree
toKey word = case word of
  "+" -> Operator Add
  "-" -> Operator Sub
  "*" -> Operator Mul
  "/" -> Operator Div
  "&" -> Operator And
  "|" -> Operator Or
  "~" -> Operator Not
  "zero?" -> Operator IsZero
  "if" -> IfKeyw
  "assume" -> AssumeKeyw
  otherwise -> error "Invalid syntax"


type Predicate = [ParseTree] -> Bool
type Reduction = [ParseTree] -> ParseTree

data Rule = Rule Predicate Reduction

apply :: Rule -> [ParseTree] -> Maybe (ParseTree)
apply (Rule predicate reduce) seq =
  case (predicate seq) of
    True  -> Just (reduce seq)
    False -> Nothing
predBinaryPrimApp :: Predicate
predBinaryPrimApp ls =
   let
     preds = [(isEqv LBracket),
              (isEqv (Operator Add)),
              isExpr, isExpr,
              (isEqv RBracket)]
   in
     (checkLen ls 5) && (seqMatch preds ls)
 
redBinaryPrimApp :: Reduction
redBinaryPrimApp ls = Appl op [x, y]
  where
    (Operator op) = ls !! 1
    x  = ls !! 2
    y  = ls !! 3

binaryPrimApp :: Rule
binaryPrimApp = Rule predBinaryPrimApp redBinaryPrimApp

predUnaryPrimApp :: Predicate
predUnaryPrimApp ls =
  let
    preds = [(isEqv LBracket),
             (isEqv (Operator Add)),
             isExpr,
             (isEqv RBracket)]
  in
    (checkLen ls 4) && (seqMatch preds ls)

redUnaryPrimApp :: Reduction
redUnaryPrimApp ls = Appl op [x]
  where
    (Operator op) = ls !! 1
    x  = ls !! 2

unaryPrimApp :: Rule
unaryPrimApp = Rule predUnaryPrimApp redUnaryPrimApp

predBind :: Predicate
predBind ls =
  let
    preds = [(isEqv LBracket),
             (isEqv (Symbol "x")),
             isExpr,
             (isEqv RBracket)]
  in
    (checkLen ls 4) && (seqMatch preds ls)

redBind :: Reduction
redBind ls = BindAppl sym expr
  where
    sym = ls !! 1
    expr  = ls !! 2

bind :: Rule
bind = Rule predBind redBind


predListBinds :: Predicate
predListBinds ls =
  let
    preds = [(isEqv (BindAppl (Symbol "x") (Numeric 1))),
             (isEqv (BindAppl (Symbol "x") (Numeric 1)))
            ]
  in
    (checkLen ls 2) && (seqMatch preds ls)

redListBinds :: Reduction
redListBinds ls = BindSeq ls

listBinds :: Rule
listBinds = Rule predListBinds redListBinds


predAppendBinds :: Predicate
predAppendBinds ls =
  let
    preds = [(isEqv (BindSeq [BindAppl (Symbol "x") (Numeric 1)])),
             (isEqv (BindAppl (Symbol "x") (Numeric 1)))
            ]
  in
    (checkLen ls 2) && (seqMatch preds ls)

redAppendBinds :: Reduction
redAppendBinds ls = BindSeq $ oldBinds ++ [newBind]
  where
    getBindsFromSeq (BindSeq binds) = binds
    oldBinds = getBindsFromSeq $ ls !! 0
    newBind = ls !! 1

appendBinds :: Rule
appendBinds = Rule predAppendBinds redAppendBinds


predAssumeBinds :: Predicate
predAssumeBinds ls =
  let
    preds = [(isEqv LBracket),
             (isEqv (BindSeq [BindAppl (Symbol "x") (Numeric 1)])),
             (isEqv RBracket)
            ]
  in
    (checkLen ls 3) && (seqMatch preds ls)

redAssumeBinds :: Reduction
redAssumeBinds ls = BindSeq $ allBinds
  where
    getBindsFromSeq (BindSeq binds) = binds
    allBinds = getBindsFromSeq $ ls !! 1

assumeBinds :: Rule
assumeBinds = Rule predAssumeBinds redAssumeBinds


predAssumeBind :: Predicate
predAssumeBind ls =
  let
    preds = [(isEqv LBracket),
             (isEqv (BindAppl (Symbol "x") (Numeric 1))),
             (isEqv RBracket)
            ]
  in
    (checkLen ls 3) && (seqMatch preds ls)

redAssumeBind :: Reduction
redAssumeBind ls = BindSeq $ [bind]
  where
    bind = ls !! 1

assumeBind :: Rule
assumeBind = Rule predAssumeBind redAssumeBind


predAssume :: Predicate
predAssume ls =
  let
    preds = [(isEqv LBracket),
             (isEqv AssumeKeyw),
             (isEqv (BindSeq [BindAppl (Symbol "x") (Numeric 1)])),
             isExpr,
             (isEqv RBracket)
            ]
  in
    (checkLen ls 5) && (seqMatch preds ls)

redAssume :: Reduction
redAssume ls = AssumeAppl binds expr
  where
    binds = ls !! 2
    expr = ls !! 3

assume :: Rule
assume = Rule predAssume redAssume

predIf :: Predicate
predIf ls =
  let
    preds = [(isEqv LBracket),
             (isEqv IfKeyw),
             isExpr,
             isExpr,
             isExpr,
             (isEqv RBracket)
            ]
  in
    (checkLen ls 6) && (seqMatch preds ls)

redIf :: Reduction
redIf ls = IfAppl cond thenExpr elseExpr
  where
    cond = ls !! 2
    thenExpr = ls !! 3
    elseExpr = ls !! 4
        
rediff :: Rule
rediff = Rule predIf redIf


keywords :: [String]
keywords = ["+", "-", "/", "*", "&", "|", "~", "zero?",
            "assume", "if", "id-ref", "(", ")", "True", "False"]

tokenIter :: String -> String -> [String] -> [String]
tokenIter expr word tokens
  | (expr == "") =
    case word of
      ""        -> tokens
      otherwise -> tokens ++ [word]
  | (word `elem` keywords) = tokenIter expr "" (tokens ++ [word])  
  | ((head expr) == ' ') =
      case word of
        ""        -> tokenIter (tail expr) word tokens
        otherwise -> tokenIter (tail expr) "" (tokens ++ [word])
  | otherwise = tokenIter (tail expr) (word ++ [(head expr)]) tokens

tokenize :: String -> [String]
tokenize s = tokenIter s "" []
    
windowmap :: Int
          -> Rule
          -> [ParseTree] -- input sequence of tokens
          -> [ParseTree] -- accumulator
          -> [ParseTree] -- output (reduced) token sequence
windowmap n rule acc [] = acc
windowmap n rule acc ls =
  let
    window = take n ls
    rest = drop n ls
    reduced = apply rule window
  in
    case reduced of
      Just x  -> windowmap n rule (acc ++ [x]) rest
      Nothing -> windowmap n rule (acc ++ [head ls]) (tail ls)

rules :: [(Int, Rule)]
rules = [(4, unaryPrimApp),
         (5, binaryPrimApp),
         (4, bind),
         (2, listBinds),
         (2, appendBinds),
         (3, assumeBinds),
         (3, assumeBind),
         (5, assume),
         (6, rediff)]

mapRule :: (Int, Rule) -> [ParseTree] -> [ParseTree]
mapRule (size, rule) seq = windowmap size rule [] seq

recurReduce :: Int -> [ParseTree] -> ParseTree
recurReduce 200 ls = error "Error: sentence is unparseable"
recurReduce iters [a] = a
recurReduce iters ls =
  let
    itern = foldr mapRule ls rules  
  in
    recurReduce (iters + 1) itern
