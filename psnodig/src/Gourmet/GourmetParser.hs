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
        , "!=", "while", "{", "}", "if", "func", "("
        , ")", ">=", "<=", "true", "false", "return"
        , "[", "]", "else", "&&", "||", "!", "for"
        , ",", "contains", "length", "ceil", "floor"
        , ":", "#", "@", "break", "continue", "struct"
        , "not", "%"
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

-- Parse structs

parseStruct :: Parser Struct
parseStruct =
    Struct <$> (reservedOp "struct" *> identifier) <* reservedOp "{"
           <*> parseArgument `sepBy` (whiteSpace >> char ',' >> whiteSpace)
           <* reservedOp "}"

parseStructField :: Parser StructField
parseStructField =
    StructField <$> identifier <* char '.' <*> identifier

-- would much prefer () over {}, but with (), this is identical to function call
parseStructAssignment :: Parser StructAssignment
parseStructAssignment =
    StructAssignment <$> identifier <* reservedOp "{"
                     <*> parseExpr `sepBy` (whiteSpace >> char ',' >> whiteSpace)
                     <* reservedOp "}"

-- Parse expressions

parseExpr :: Parser Expression
parseExpr = buildExpressionParser table term
    where
        table = [ [ Infix (BinaryExp Times <$ reservedOp "*") AssocLeft
                    , Infix (BinaryExp Division <$ reservedOp "/") AssocLeft
                    , Infix (BinaryExp Modulo <$ reservedOp "%") AssocLeft ]
                , [ Infix (BinaryExp Plus <$ reservedOp "+") AssocLeft
                    , Infix (BinaryExp Minus <$ reservedOp "-") AssocLeft ]
                , [ Infix (BinaryExp LessThan <$ reservedOp "<") AssocNone
                    , Infix (BinaryExp LessThanEqual <$ reservedOp "<=") AssocNone
                    , Infix (BinaryExp GreaterThan <$ reservedOp ">") AssocNone
                    , Infix (BinaryExp GreaterThanEqual <$ reservedOp ">=") AssocNone ]
                , [ Infix (BinaryExp Equal <$ reservedOp "==") AssocNone
                    , Infix (BinaryExp NotEqual <$ reservedOp "!=") AssocNone ]
                , [ Infix (BinaryExp And <$ reservedOp "&&") AssocLeft
                    , Infix (BinaryExp Or <$ reservedOp "||") AssocLeft ]
                ]
        term = choice
            [ try parseNotExp
            , try parseStructFieldExp
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
                    Not <$> (reservedOp "not" *> parseExpr)
                parseStructFieldExp =
                    StructFieldExp <$> parseStructField
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

parseArray :: Parser Array
parseArray =
    Array <$> (reservedOp "[" *> parseExpr `sepBy` (whiteSpace >> char ',' >> whiteSpace)) <* reservedOp "]"

-- Parse functions

parseArgument :: Parser Argument
parseArgument = try parseArrayArg <|> parseStringArg
    where parseArrayArg = ArrayArg <$> identifier <*> identifier <* (whiteSpace >> char '*' >> whiteSpace)
          parseStringArg = SingleArg <$> identifier <*> identifier

parseFunction :: Parser Function
parseFunction =
    Function
        <$> (reservedOp "func" *> identifier) <* reservedOp "("
        <*> parseArgument `sepBy` (whiteSpace >> char ',' >> whiteSpace)
        <* reservedOp ")" <* reservedOp "{"
        <*> parseStmt `endBy` whiteSpace <* reservedOp "}"

parseFunctionCall :: Parser FunctionCall
parseFunctionCall =
    FunctionCall
        <$> identifier <* reservedOp "("
        <*> parseExpr `sepBy` (whiteSpace >> char ',' >> whiteSpace) <* reservedOp ")"

parseAssignmentTarget :: Parser AssignmentTarget
parseAssignmentTarget = choice
    [ try parseStructFieldTarget
    , try parseArrayIndexTarget
    , parseVariableTarget
    ]
    where
        parseStructFieldTarget = StructFieldTarget <$> parseStructField
        parseArrayIndexTarget = ArrayIndexTarget <$> identifier <* reservedOp "[" <*> parseExpr <* reservedOp "]"
        parseVariableTarget = VariableTarget <$> identifier

parseAssignmentValue :: Parser AssignmentValue
parseAssignmentValue = try parseStructValue <|> parseExpressionValue
    where
        parseStructValue = StructValue <$> parseStructAssignment
        parseExpressionValue = ExpressionValue <$> parseExpr

-- Parse statements

parseStmt :: Parser Statement
parseStmt = choice
    [ try parseBreakStmt
    , try parseContinueStmt
    , try parseAnnotationStmt
    , try parseHashStmt
    , try parseAssignment
    , try whileStmt
    , try forEachStmt
    , try forStmt
    , try ifStmt
    , try returnStmt
    , try parseFunctionCallStmt
    ] <?> "Expected statement!"
    where
        parseBreakStmt = 
            reservedOp "break" *> pure Break
        parseContinueStmt =
            reservedOp "continue" *> pure Continue
        parseAnnotationStmt =
            AnnotationStmt
                <$> (reservedOp "@" *> reservedOp "{" *> manyTill anyChar (try (reservedOp "}")))
                <*> (reservedOp "{" *> many parseStmt <* reservedOp "}")
        parseHashStmt =
            HashStmt <$> (reservedOp "#" *> parseStmt)
        parseFunctionCallStmt =
            CallStmt <$> parseFunctionCall
        parseAssignment =
            Assignment
                <$> parseAssignmentTarget <* reservedOp ":="
                <*> parseAssignmentValue
        whileStmt =
            Loop
                <$> (reservedOp "while" *> parseExpr) <* reservedOp "{"
                <*> many parseStmt <* reservedOp "}"
        forEachStmt =
            ForEach
                <$> (reservedOp "for" *> identifier) <* reservedOp ":="
                <*> parseExpr <* reservedOp "{"
                <*> many parseStmt <* reservedOp "}"
        forStmt =
            For
                <$> (reservedOp "for" *> identifier) <* reservedOp ":="
                <*> parseExpr <* reservedOp ","
                <*> parseExpr <* reservedOp "{"
                <*> many parseStmt <* reservedOp "}"
        ifStmt =
            If
                <$> (reservedOp "if" *> parseExpr) <* reservedOp "{"
                <*> many parseStmt <* reservedOp "}"
                <*> optionMaybe parseElse
        returnStmt =
            Return <$> (reservedOp "return" *> parseExpr)

parseElse :: Parser Else
parseElse = (try parseElseIf) <|> parsePlainElse
    where
        parseElseIf =
            ElseIf
                <$> (reservedOp "else" *> reservedOp "if" *> parseExpr) <* reservedOp "{"
                <*> many parseStmt <* reservedOp "}"
                <*> optionMaybe parseElse
        parsePlainElse =
            Else <$> (reservedOp "else" *> reservedOp "{" *> many parseStmt) <* reservedOp "}"

parseGourmet :: Parser Program
parseGourmet = do
    whiteSpace
    structs <- many parseStruct
    funcs <- many1 parseFunction
    functioncall <- parseFunctionCall
    return $ Program structs funcs functioncall
