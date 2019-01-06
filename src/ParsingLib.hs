module ParsingLib where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
  deriving (Show)

data BBinOp
  = And
  | Or
  deriving (Show)

data RBinOp
  = Greater
  | Less
  deriving (Show)

data AExpr
  = IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  deriving (Show)

data ABinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

data Stmt
  = A AExpr
  | B BExpr
  deriving (Show)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'parens' parses something between parenthesis.

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'integer' parses an integer.

integer :: Parser Integer
integer = lexeme L.decimal

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["True","False","not","and","or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

whileParser :: Parser Stmt
whileParser = between sc eof stmt

stmt :: Parser Stmt
stmt = arithmeticStmt
  <|> booleanStmt
  <|> parens stmt

arithmeticStmt :: Parser Stmt
arithmeticStmt = do
  expr <- aExpr
  return $ A expr

booleanStmt :: Parser Stmt
booleanStmt = do
  expr <- bExpr
  return $ B expr

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-") ]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide   <$ symbol "/") ]
  , [ InfixL (ABinary Add      <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-") ]
  ]

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [Prefix (Not <$ rword "not") ]
  , [InfixL (BBinary And <$ rword "and")
    , InfixL (BBinary Or <$ rword "or") ]
  ]

aTerm :: Parser AExpr
aTerm = parens aExpr <|> IntConst <$> integer

bTerm :: Parser BExpr
bTerm =  parens bExpr
  <|> (BoolConst True  <$ rword "True")
  <|> (BoolConst False <$ rword "False")
  <|> rExpr

rExpr :: Parser BExpr
rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return (RBinary op a1 a2)

relation :: Parser RBinOp
relation = (Greater <$ symbol ">")
  <|> (Less <$ symbol "<")

-- Evaluation
evalBExpr :: BExpr -> Bool
evalBExpr (BoolConst x) = x
evalBExpr (Not x) = not (evalBExpr x)
evalBExpr (BBinary And x y)     = (evalBExpr x) && (evalBExpr y)
evalBExpr (BBinary Or x y)      = (evalBExpr x) || (evalBExpr y)
evalBExpr (RBinary Greater x y) = (evalAExpr x) >  (evalAExpr y)

evalAExpr :: AExpr -> Integer
evalAExpr (IntConst x) = x
evalAExpr (Neg x) = - evalAExpr x
evalAExpr (ABinary Add x y) = (evalAExpr x) + (evalAExpr y)
evalAExpr (ABinary Subtract x y) = (evalAExpr x) - (evalAExpr y)
evalAExpr (ABinary Multiply x y) = (evalAExpr x) * (evalAExpr y)
evalAExpr (ABinary Divide x y) = div (evalAExpr x) (evalAExpr y)

evalStmt :: Stmt -> String
evalStmt (A x) = show $ evalAExpr x
evalStmt (B y) = show $ evalBExpr y

parseAndEval :: String -> Either String String
parseAndEval "" = Right ""
parseAndEval x  = case res of
                    (Right x) -> Right $ evalStmt x
                    (Left  y) -> Left  $ errorBundlePretty y
  where res = parse whileParser "" x
