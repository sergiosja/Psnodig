module Gourmet.GourmetParser (parseGourmet) where

import Syntax
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Expr (buildExpressionParser, Assoc(..), Operator(..))
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef {
           Token.identStart = letter,
           Token.identLetter = alphaNum <|> char '\'',
           Token.reservedOpNames =
            [ ":=", "+", "-", "*", "/", "<", ">", "=="
            , "!=", "fmt.Println", "while", "{", "}", "if"
            , "pass", "func", "(", ")", ">=", "<=", "true"
            , "false", "return", "[", "]"
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

{- Parsing -}

parseExpr :: Parser Expression
parseExpr = buildExpressionParser table term
    where
        table = [ [ Infix (reservedOp "*" >> return (BinaryExp Times)) AssocLeft
                    , Infix (reservedOp "/" >> return (BinaryExp Division)) AssocLeft]
                , [ Infix (reservedOp "+" >> return (BinaryExp Plus)) AssocLeft
                    , Infix (reservedOp "-" >> return (BinaryExp Minus)) AssocLeft]
                , [ Infix (reservedOp "==" >> return (BinaryExp Equal)) AssocNone
                    , Infix (reservedOp "!=" >> return (BinaryExp NotEqual)) AssocNone
                    , Infix (reservedOp "<" >> return (BinaryExp LessThan)) AssocNone
                    , Infix (reservedOp "<=" >> return (BinaryExp LessThanEqual)) AssocNone
                    , Infix (reservedOp ">" >> return (BinaryExp GreaterThan)) AssocNone
                    , Infix (reservedOp ">=" >> return (BinaryExp GreaterThanEqual)) AssocNone]
                ]
        term = choice
            [ try parseArray
            , try parseArrayExp
            , try parseFunctionCallExp
            , try parseBool
            , try parseVariableExp
            , try parseConstant
            , try $ parens parseExpr
            ]
            where
                parseArray = do
                    reservedOp "["
                    entries <- parseExpr `sepBy` (whiteSpace >> char ',' >> whiteSpace)
                    reservedOp "]"
                    return $ Array entries
                parseArrayExp = do
                    array <- identifier
                    reservedOp "["
                    index <- parseExpr
                    reservedOp "]"
                    return $ ArrayExp array index
                parseFunctionCallExp = do
                    functioncall <- parseFunctionCall
                    return $ Call functioncall
                parseVariableExp = do
                    var <- identifier
                    return (VariableExp var)
                parseConstant = do
                    value <- integer
                    return (Constant (fromIntegral value))
                parseBool = parseTrue <|> parseFalse
                    where
                        parseTrue = do
                            reservedOp "true"
                            return (Boolean True)
                        parseFalse = do
                            reservedOp "false"
                            return (Boolean False)

parseFunction :: Parser Function
parseFunction = do
    reservedOp "func"
    funcname <- identifier
    reservedOp "("
    args <- identifier `sepBy` (whiteSpace >> char ',' >> whiteSpace)
    reservedOp ")"
    reservedOp "{"
    stmts <- parseStmt `endBy` whiteSpace
    reservedOp "}"
    return $ Function funcname args stmts

parseFunctionCall :: Parser FunctionCall
parseFunctionCall = do
    funcname <- identifier
    reservedOp "("
    args <- parseExpr `sepBy` (whiteSpace >> char ',' >> whiteSpace)
    reservedOp ")"
    return (FunctionCall funcname args)

parseStmt :: Parser Statement
parseStmt = choice
    [ try parseAssignment
    , try loopStmt
    , try ifStmt
    , try passStmt
    , try returnStmt
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
            statements <- many1 parseStmt
            reservedOp "}"
            return (Loop cond statements)
        ifStmt = do
            reservedOp "if"
            cond <- parseExpr
            reservedOp "{"
            statements <- many1 parseStmt
            reservedOp "}"
            return (If cond statements)
        passStmt = do
            reservedOp "pass"
            return Pass
        returnStmt = do
            reservedOp "return"
            expr <- parseExpr
            return (Return expr)
        printStmt = do
            reservedOp "fmt.Println"
            expr <- parens parseExpr
            return (Print expr)

parseGourmet :: Parser Program
parseGourmet = do
    whiteSpace
    funcs <- many1 {- - -} parseFunction -- <* whiteSpace)
    functioncall <- parseFunctionCall
    return $ Program funcs functioncall
