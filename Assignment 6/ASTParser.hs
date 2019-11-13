
module ASTParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


type ID = String

data Binding = Bind ID AST
               deriving Show

data FBind = FBind ID [ID] AST
             deriving (Show)
  
data AST =
    Number Int
  | Boolean Bool                              
  | Reference ID                              
  | Assume [Binding] AST                      
  | If AST AST AST                            
  | Function [ID] AST
  | RecFun [FBind] AST
  | App [AST]  -- 'app' meaning 'Application'
  | NewRef AST
  | DeRef AST
  | SetRef AST AST
  | Sequence [AST]
  deriving Show
languageDef =
   emptyDef { Token.identStart      = letter <|> oneOf "+-*/|&~"
            , Token.identLetter     = alphaNum <|> oneOf "+-*/|&~"
            , Token.opLetter        = oneOf "+-*/&|~" 
            , Token.reservedNames   = [ "if", "assume", "True", "False", "function", "newref", "deref", "setref", "seq", "recfun"]
            }

lexer = Token.makeTokenParser languageDef

-- specific token parsers
parens = Token.parens lexer -- parentheses
identifier =    (Token.identifier lexer)
            <|> (Token.operator lexer)
                -- identifier
reserved   = Token.reserved lexer -- reserved name
integer    = Token.natural    lexer -- integer
whiteSpace = Token.whiteSpace lexer --  whitespace
symbol = Token.symbol lexer ","
dot = Token.dot lexer


astParser :: Parser AST
astParser =    integerParser
           <|> booleanTrue
           <|> booleanFalse
           <|> identifierParser
           <|> parens astInParens

-- to parse complex AST expressions within parentheses
astInParens :: Parser AST
astInParens =    assumeParser
             <|> ifParser
             <|> funcParser
             <|> appParser
             <|> recFunParser
             <|> newRefParser
             <|> deRefParser
             <|> setRefParser
             <|> seqParser

integerParser :: Parser AST
integerParser = do
  val <- integer
  return $ Number $ fromIntegral val

booleanTrue :: Parser AST
booleanTrue = do
  reserved "True"
  return $ Boolean True

booleanFalse :: Parser AST
booleanFalse = do
  reserved "False"
  return $ Boolean False

identifierParser :: Parser AST
identifierParser = do
  id <- identifier
  return $ Reference id

bindListParser :: Parser [Binding]
bindListParser = do
  bindList <- (sepBy1 (parens bindParser) dot)
  return bindList
  
bindParser :: Parser Binding
bindParser = do
  id <- identifier
  expr <- astParser
  return $ Bind id expr

assumeParser :: Parser AST
assumeParser = do
  reserved "assume"
  bindings <- parens bindListParser
  expr <- astParser
  return $ Assume bindings expr



ifParser :: Parser AST
ifParser = do
    reserved "if"
    cond <- astParser
    thenExpr <- astParser
    elseExpr <- astParser
    return $ If cond thenExpr elseExpr



funcParser :: Parser AST
funcParser = do
  reserved "function"
  argList <- parens argParser
  body <- astParser
  return $ Function argList body

argParser :: Parser [ID]
argParser = do
  argList <- (sepBy1 identifier dot)
  return argList

-- to parse fbinds
fbindListParser :: Parser [FBind]
fbindListParser = do
  fbindList <- (sepBy1 (parens fbindParser) dot)
  return fbindList
  
fbindParser :: Parser FBind
fbindParser = do
  id <- identifier
  argList <- parens argParser
  body <- astParser
  return $ FBind id argList body
  
recFunParser :: Parser AST
recFunParser = do
    reserved "recfun"
    fbinds <- parens fbindListParser
    body <- astParser
    return $ RecFun fbinds body


appParser :: Parser AST
appParser = do
  exprList <- (sepBy1 astParser whiteSpace)
  return $ App exprList

newRefParser :: Parser AST
newRefParser = do
      reserved "newref"
      ref <- astParser
      return $ NewRef ref


deRefParser :: Parser AST
deRefParser = do
      reserved "deref"
      ref <- astParser
      return $ DeRef ref


setRefParser :: Parser AST
setRefParser = do
      reserved "setref"
      ref <- astParser
      val <- astParser
      return $ SetRef ref val


-- to parse a list of ASTs
astListParser :: Parser [AST]
astListParser = do
  astList <- (sepBy1 astParser dot)
  return astList


seqParser :: Parser AST
seqParser = do
  reserved "seq"
  seq <- parens astListParser
  return $ Sequence seq

parseString :: String -> AST
parseString str =
  case parse astParser "" str of
     Left e  -> error $ show e
     Right r -> r
