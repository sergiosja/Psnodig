module Petite.PetiteParser (parsePetite) where

import Syntax
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef {
           Token.identStart = letter,
           Token.identLetter = alphaNum <|> char '\'',
           Token.reservedOpNames =
            [ "=", "+", "-", "*", "/", "<", ">"
            , "==", "!=", "print", "while", ":", "if"
            ]
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
    , try ifStmt
    , printStmt
    ]
    where
        parseAssignment = do
            var <- identifier
            reservedOp "="
            expr <- parseExpr
            return (Assignment var expr)
        loopStmt = do
            reservedOp "while"
            cond <- parseExpr
            reservedOp ":"
            statements <- many parseStmt
            return (Loop cond statements)
        ifStmt = do
            reservedOp "if"
            cond <- parseExpr
            reservedOp ":"
            statements <- many parseStmt
            return (If cond statements)
        printStmt = do
            reservedOp "print"
            expr <- parens parseExpr
            return (Print expr)

parsePetite :: Parser Program
parsePetite = do
    whiteSpace
    statements <- parseStmt `endBy` whiteSpace
    return (Program statements)
