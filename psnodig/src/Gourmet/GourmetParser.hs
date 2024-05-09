module Gourmet.GourmetParser
    ( parseGourmet
    , parseValue
    , parseProgramDescription
    , parseStmt
    , parseExpr
    , parseStructDecl
    ) where

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
        , "!=", "{", "}", "(", ")", ">=", "<="
        , "[", "]", "&&", "||", "!", ",", ":", "#"
        , "@", "%", "."
        ],
    Token.reservedNames =
        [ "while", "if", "func", "true", "false"
        , "return", "else", "for", "break", "set"
        , "map", "not", "struct", "continue"
        ],
    Token.commentStart = "/*",
    Token.commentEnd = "*/",
    Token.commentLine = "//",
    Token.nestedComments = False
}

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

integer :: Parser Integer
integer = Token.integer lexer

decimal :: Parser Double
decimal = Token.float lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

parens :: Parser Expression -> Parser Expression
parens = Token.parens lexer

comma :: Parser ()
comma = whiteSpace >> char ',' >> whiteSpace

colon :: Parser ()
colon = whiteSpace >> char ':' >> whiteSpace

{- Parsing -}

sWhiteSpace :: Char -> Parser ()
sWhiteSpace c = whiteSpace >> char c >> whiteSpace

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
    StructField <$> parseExpr1 <* sWhiteSpace '.'
                <*> parseExpr

-- parseStructFieldExpr :: Parser Expression
-- parseStructFieldExpr = try (StructFieldExp <$> parseStructField) <|> parseExpr

-- update! jeg må ha parseStructFieldExpr blant structFields :((
    -- må altså refaktorere en del! og knekke denne koden på et vis


-- Values -- hvis noe går galt: bytt fra reservedOp til char!!!

parseValue :: Parser Value
parseValue = choice
    [ try parseNil
    , try parseHashMap
    , try parseHashSet
    , try parseBool
    , try parseDecimal
    , try parseNumber
    , try parseText
    , try parseList
    ]
    where
        parseNil = reservedOp "nil" *> pure Nil
        parseBool =
            Boolean <$> (reservedOp "true" *> pure True
                    <|> reservedOp "false" *> pure False)
        parseDecimal = Decimal <$> decimal
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
            [ try parseFunctionCallExp
            , try parseListIndexExp
            , try parseStructFieldExpr
            , try parseNotExp
            , try parseStructExpr
            , try parseConstant
            , try parseVariableExp
            , try $ parens parseExpr
            ]
            where
                parseStructFieldExpr =
                    {-try-} (StructFieldExp <$> parseStructField) -- <|> parseExpr1
                parseNotExp =
                    Not <$> (reservedOp "not" *> parseExpr)
                parseListIndexExp =
                    ListIndex <$> identifier <*> many1 (sWhiteSpace '[' *> parseExpr <* sWhiteSpace ']')
                parseFunctionCallExp =
                    CallExp <$> parseFunctionCall
                parseVariableExp =
                    VariableExp <$> identifier
                parseConstant =
                    Constant <$> parseValue
                parseStructExpr =
                    StructExpr <$> parseStruct

parseExpr1 :: Parser Expression
parseExpr1 = buildExpressionParser table term
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
                , [ Infix (BinaryExp And <$ reservedOp "&&") AssocLeft ]
                , [ Infix (BinaryExp Or <$ reservedOp "||") AssocLeft ]
                ]
        term = choice
            [ try parseFunctionCallExp
            , try parseNotExp
            , try parseVariableExp
            , try parseStructExpr
            , try parseConstant
            , try parseListIndexExp
            , try $ parens parseExpr
            ]
            where
                parseNotExp =
                    Not <$> (reservedOp "not" *> parseExpr)
                parseListIndexExp =
                    ListIndex <$> identifier <*> many1 (sWhiteSpace '[' *> parseExpr <* sWhiteSpace ']')
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
        <$> (reserved "func" *> identifier) <* reservedOp "("
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
    [ try parseHashStmt
    , try parseAnnotationStmt
    , try parseBreakStmt
    , try parseContinueStmt
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

parseProgramDescription :: Parser ProgramDescription
parseProgramDescription = ProgramDescription
    <$> (char '?' *> whiteSpace *> manyTill anyChar (char '?'))
    <* whiteSpace <*> (char '!' *> whiteSpace *> manyTill anyChar (char '!'))
    <* whiteSpace

parseGourmet :: Parser Program
parseGourmet = do
    whiteSpace
    programDescription <- optionMaybe parseProgramDescription
    structs <- many parseStructDecl
    funcs <- many parseFunction
    functioncall <- optionMaybe parseFunctionCall
    return $ Program programDescription structs funcs functioncall
