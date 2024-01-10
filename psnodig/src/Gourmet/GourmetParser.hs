module Gourmet.GourmetParser (parseGourmet) where

import Syntax
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Expr (buildExpressionParser, Assoc(..), Operator(..))
import qualified Text.Parsec.Token as Token
import qualified Data.Map as Map
import qualified Data.Set as Set

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
        , "not", "%", "map", "set", "int", "str", "bool"
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

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

parens :: Parser Expression -> Parser Expression
parens = Token.parens lexer

comma :: Parser ()
comma = whiteSpace >> char ',' >> whiteSpace

colon :: Parser ()
colon = whiteSpace >> char ':' >> whiteSpace

{- Parsing -}

-- Parse structs

parseStructDecl :: Parser StructDecl
parseStructDecl =
    StructDecl <$> (reservedOp "struct" *> identifier) <* reservedOp "{"
           <*> parseArgument `sepBy` comma <* reservedOp "}"

parseStruct :: Parser Struct
parseStruct =
    Struct <$> (reservedOp "struct" *> identifier)
           <*> (reservedOp "(" *> parseExpr `sepBy` comma <* reservedOp ")")

parseStructField :: Parser StructField
parseStructField =
    StructField <$> parseExpr <* char '.' <*> parseFieldExpr

parseFieldExpr :: Parser Expression
parseFieldExpr = try parseStructFieldExpr <|> parseExpr

parseStructFieldExpr :: Parser Expression
parseStructFieldExpr = StructFieldExp <$> parseStructField


-- Values -- hvis noe går galt: bytt fra reservedOp til char!!!

parseValue :: Parser Value
parseValue = choice
    [ try parseHashMap
    , try parseHashSet
    , try parseNil
    , try parseBool
    , try parseNumber
    , try parseText
    , try parseList
    -- , try parseStructVal
    ]
    where
        parseNil = reservedOp "nil" *> pure Nil
        parseBool =
            Boolean <$> (reservedOp "true" *> pure True
                    <|> reservedOp "false" *> pure False)
        parseNumber = Number <$> integer
        parseText = Text <$> stringLiteral
        parseList =
            List <$> (reservedOp "[" *> parseExpr `sepBy` comma <* reservedOp "]")
        parseHashMap =
            HashMap . Map.fromList
                <$> (reservedOp "map" *> reservedOp "{" *> parsePair `sepBy` comma <* reservedOp "}")
        parseHashSet =
            HashSet . Set.fromList
                <$> (reservedOp "set" *> reservedOp "{" *> parseExpr `sepBy` comma <* reservedOp "}")
        -- parseStructVal =
            -- ...

parsePair :: Parser (Expression, Expression)
parsePair = (,) <$> parseExpr <* colon <*> parseExpr

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
            , try parseStructExpr
            , try parseConstant
            , try parseListIndexExp
            , try parseFunctionCallExp
            , try parseVariableExp
            , try $ parens parseExpr
            ]
            where
                parseNotExp =
                    Not <$> (reservedOp "not" *> parseExpr)
                parseListIndexExp =
                    ListIndex <$> identifier <*> many1 (whiteSpace *> char '[' *> parseExpr <* char ']' <* whiteSpace)
                parseFunctionCallExp =
                    CallExp <$> parseFunctionCall
                parseVariableExp =
                    VariableExp <$> identifier
                parseConstant =
                    Constant <$> parseValue
                parseStructExpr =
                    StructExpr <$> parseStruct

-- Parse functions

parseArgument :: Parser Argument
parseArgument =
    Argument <$> identifier <*> identifier

parseFunction :: Parser Function
parseFunction =
    Function
        <$> (reservedOp "func" *> identifier) <* reservedOp "("
        <*> parseArgument `sepBy` comma
        <* reservedOp ")" <* reservedOp "{"
        <*> parseStmt `endBy` whiteSpace <* reservedOp "}"

parseFunctionCall :: Parser FunctionCall
parseFunctionCall =
    FunctionCall
        <$> identifier <* reservedOp "("
        <*> parseExpr `sepBy` comma <* reservedOp ")"

parseAssignmentTarget :: Parser AssignmentTarget
parseAssignmentTarget = choice
    [ try parseStructFieldTarget
    , try parseListIndexTarget
    , parseVariableTarget
    ]
    where
        parseStructFieldTarget = StructFieldTarget <$> parseStructField
        parseListIndexTarget = ListIndexTarget <$> identifier <*> many1 (reservedOp "[" *> parseExpr <* reservedOp "]")
        parseVariableTarget = VariableTarget <$> identifier

parseAssignmentValue :: Parser AssignmentValue
parseAssignmentValue = try parseStructValue <|> parseExpressionValue
    where
        parseStructValue = StructValue <$> parseStruct
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
    ] <?> "a statement."
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
    structs <- many parseStructDecl
    funcs <- many1 parseFunction
    functioncall <- parseFunctionCall
    return $ Program structs funcs functioncall
