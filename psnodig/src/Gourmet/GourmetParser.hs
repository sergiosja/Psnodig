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
            , "!=", "while", "{", "}", "if"
            , "pass", "func", "(", ")", ">=", "<=", "true"
            , "false", "return", "[", "]", "else", "&&"
            , "||", "!", "for", ",", "contains", "length"
            , "ceil", "floor"
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
        table = [ [ Infix (BinaryExp Times <$ reservedOp "*") AssocLeft
                    , Infix (BinaryExp Division <$ reservedOp "/") AssocLeft]
                , [ Infix (BinaryExp Plus <$ reservedOp "+") AssocLeft
                    , Infix (BinaryExp Minus <$ reservedOp "-") AssocLeft]
                , [ Infix (BinaryExp And <$ reservedOp "&&") AssocLeft
                    , Infix (BinaryExp Or <$ reservedOp "||") AssocLeft ]
                , [ Infix (BinaryExp Equal <$ reservedOp "==") AssocNone
                    , Infix (BinaryExp NotEqual <$ reservedOp "!=") AssocNone
                    , Infix (BinaryExp LessThan <$ reservedOp "<") AssocNone
                    , Infix (BinaryExp LessThanEqual <$ reservedOp "<=") AssocNone
                    , Infix (BinaryExp GreaterThan <$ reservedOp ">") AssocNone
                    , Infix (BinaryExp GreaterThanEqual <$ reservedOp ">=") AssocNone]
                ]
        term = choice
            [ try parseNotExp
            , try parseArrayExp
            , try parseArrayIndexExp
            , try parseFunctionCallExp
            , try parseBool
            , try parseVariableExp
            , try parseConstant
            , try $ parens parseExpr
            ]
            where
                parseNotExp =
                    Not <$> (reservedOp "!" *> parseExpr)
                parseArrayExp =
                    ArrayExp <$> parseArray
                parseArrayIndexExp =
                    ArrayIndex <$> identifier <* reservedOp "[" <*> parseExpr <* reservedOp "]"
                parseFunctionCallExp =
                    CallExp <$> parseFunctionCall
                parseVariableExp =
                    VariableExp <$> identifier
                parseConstant =
                    Constant .fromIntegral <$> integer
                parseBool = parseTrue <|> parseFalse
                    where
                        parseTrue = Boolean True <$ reservedOp "true"
                        parseFalse = Boolean False <$ reservedOp "false"
{-
<$> betyr: bruk det som kommer nå som args
a1 >> a2 betyr: les a1 og a2 men discard res av a1
<* betyr: les men discard resultat
-}

parseArray :: Parser Array
parseArray = do
    reservedOp "["
    entries <- parseExpr `sepBy` (whiteSpace >> char ',' >> whiteSpace)
    reservedOp "]"
    return $ Array entries

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

parseAssignmentTarget :: Parser AssignmentTarget
parseAssignmentTarget = try parseArrayIndexTarget <|> parseVariableTarget
    where
        parseArrayIndexTarget = ArrayIndexTarget <$> identifier <* reservedOp "[" <*> parseExpr <* reservedOp "]"
        parseVariableTarget = VariableTarget <$> identifier

parseStmt :: Parser Statement
parseStmt = choice
    [ try parseAssignment
    , try whileStmt
    , try forEachStmt
    , try forStmt
    , try ifStmt
    , try passStmt
    , try returnStmt
    , try parseFunctionCallStmt
    ]
    where
        parseFunctionCallStmt =
            CallStmt <$> parseFunctionCall
        parseAssignment =
            Assignment <$> parseAssignmentTarget <* reservedOp ":=" <*> parseExpr
        whileStmt = do
            reservedOp "while"
            cond <- parseExpr
            reservedOp "{"
            statements <- many1 parseStmt
            reservedOp "}"
            return (Loop cond statements)
-- for <var> := <collection> {}
        forEachStmt = do
            reservedOp "for"
            var <- identifier
            reservedOp ":="
            array <- identifier
            reservedOp "{"
            statements <- many parseStmt
            reservedOp "}"
            return $ ForEach var array statements
-- for <var> := <from>, <to> [, <step>] {}
        forStmt = do
            reservedOp "for"
            var <- identifier
            reservedOp ":="
            from <- parseExpr
            reservedOp ","
            to <- parseExpr
            reservedOp "{"
            statements <- many parseStmt
            reservedOp "}"
            return $ For var from to statements
        ifStmt = do
            reservedOp "if"
            cond <- parseExpr
            reservedOp "{"
            statements <- many parseStmt
            reservedOp "}"
            elsePart <- optionMaybe parseElse
            return (If cond statements elsePart)
        passStmt =
            reservedOp "pass" *> pure Pass
        returnStmt = do
            Return <$> (reservedOp "return" *> parseExpr)

parseElse :: Parser Else
parseElse = (try parseElseIf) <|> parsePlainElse
    where
        parseElseIf = do
            reservedOp "else"
            reservedOp "if"
            expr <- parseExpr
            reservedOp "{"
            stmts <- many parseStmt
            reservedOp "}"
            elsePart <- optionMaybe parseElse
            return $ ElseIf expr stmts elsePart
        parsePlainElse = do
            reservedOp "else"
            reservedOp "{"
            stmts <- many parseStmt
            reservedOp "}"
            return $ Else stmts

parseGourmet :: Parser Program
parseGourmet = do
    whiteSpace
    funcs <- many1 {- - -} parseFunction -- <* whiteSpace)
    functioncall <- parseFunctionCall
    return $ Program funcs functioncall
