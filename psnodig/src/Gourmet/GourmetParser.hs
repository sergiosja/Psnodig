module Gourmet.GourmetParser (parseGourmet) where

import Syntax
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

{-
x := 5
while x > 0 {
    x = x - 1
    print(x)
}
-}

-- Define a lexer
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef {
           Token.identStart = letter,
           Token.identLetter = alphaNum <|> char '\'',
           Token.reservedOpNames =
            [":=", "+", "-", "*", "/", "<", ">", "==", "!=", "fmt.Println", "while", "{", "}"]
       }

identifier :: Parser String
identifier = Token.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

integer :: Parser Integer
integer = Token.integer lexer

parens :: Parser Expression -> Parser Expression
parens = Token.parens lexer

parseOp :: Parser Operator
parseOp = choice
    [ reservedOp "+" >> return Plus
    , reservedOp "-" >> return Minus
    , reservedOp "*" >> return Times
    , reservedOp "/" >> return Division
    , reservedOp "<" >> return LessThan
    , reservedOp ">" >> return GreaterThan
    , reservedOp "==" >> return Equal
    , reservedOp "!=" >> return NotEqual
    ]

parseVariableExp :: Parser Expression
parseVariableExp = do
    var <- identifier
    return (VariableExp var)

parseConstant :: Parser Expression
parseConstant = do
    value <- integer
    return (Constant (fromIntegral value))

parseBinaryExp :: Parser Expression
parseBinaryExp = try $ do
    expr <- parseTerm
    binop <- parseOp
    expr' <- parseExpr
    return (BinaryExp binop expr expr')

parseTerm :: Parser Expression
parseTerm = try parseVariableExp <|> parseConstant

parseExpr :: Parser Expression
parseExpr = choice
    [ parseBinaryExp
    , try parseVariableExp
    , parseConstant
    ]

parseStmt :: Parser Statement
parseStmt = choice
    [ try parseAssignment
    , try loopStmt
    , printStmt
    ]
    where
        parseAssignment = do
            var <- identifier
            reservedOp ":="
            expr <- parseExpr
            return (Assignment var expr)
        loopStmt = do
            reservedOp "while"
            cond <- parseExpr
            reservedOp "{"
            statements <- many parseStmt
            reservedOp "}"
            return (Loop cond statements)
        printStmt = do
            reservedOp "fmt.Println"
            expr <- parens parseExpr
            return (Print expr)

parseGourmet :: Parser Program
parseGourmet = do
    whiteSpace
    assignments <- parseStmt `endBy` whiteSpace
    return (Program assignments)
