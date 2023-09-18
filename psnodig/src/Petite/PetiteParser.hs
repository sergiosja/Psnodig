module Petite.PetiteParser (parsePetite) where

parsePetite :: Int
parsePetite = 1

-- import Syntax
-- import Text.Parsec
-- import Text.Parsec.String
-- import Text.Parsec.Language
-- import Text.Parsec.Expr (buildExpressionParser, Assoc(..), Operator(..))
-- import qualified Text.Parsec.Token as Token
-- -- import qualified Text.Parsec.Indent as Indent

-- lexer :: Token.TokenParser ()
-- lexer = Token.makeTokenParser emptyDef {
--            Token.identStart = letter,
--            Token.identLetter = alphaNum <|> char '\'',
--            Token.reservedOpNames =
--             [ "=", "+", "-", "*", "/", "<", ">"
--             , "==", "!=", "print", "while", ":", "if"
--             , "pass"
--             ]
--        }

-- identifier :: Parser String
-- identifier = Token.identifier lexer

-- reservedOp :: String -> Parser ()
-- reservedOp = Token.reservedOp lexer

-- whiteSpace :: Parser ()
-- whiteSpace = Token.whiteSpace lexer

-- integer :: Parser Integer
-- integer = Token.integer lexer

-- parens :: Parser Expression -> Parser Expression
-- parens = Token.parens lexer

-- -- indent :: Parser ()
-- -- indent = Indent.indent lexer

-- -- dedent :: Parser ()
-- -- dedent = Indent.dedent lexer

-- {- Parsing -}

-- parseExpr :: Parser Expression
-- parseExpr = buildExpressionParser table term
--     where
--         table = [ [ Infix (reservedOp "*" >> return (BinaryExp Times)) AssocLeft
--                     , Infix (reservedOp "/" >> return (BinaryExp Division)) AssocLeft]
--                 , [ Infix (reservedOp "+" >> return (BinaryExp Plus)) AssocLeft
--                     , Infix (reservedOp "-" >> return (BinaryExp Minus)) AssocLeft]
--                 , [ Infix (reservedOp "==" >> return (BinaryExp Equal)) AssocNone
--                     , Infix (reservedOp "!=" >> return (BinaryExp NotEqual)) AssocNone
--                     , Infix (reservedOp "<" >> return (BinaryExp LessThan)) AssocNone
--                     , Infix (reservedOp ">" >> return (BinaryExp GreaterThan)) AssocNone]
--                 ]
--         term = choice
--             [ try parseVariableExp
--             , parseConstant
--             , parens parseExpr
--             ]
--             where
--                 parseVariableExp = do
--                     var <- identifier
--                     return (VariableExp var)

--                 parseConstant = do
--                     value <- integer
--                     return (Constant (fromIntegral value))

-- parseStmt :: Parser Statement
-- parseStmt = choice
--     [ try parseAssignment
--     , try loopStmt
--     , try ifStmt
--     , try passStmt
--     , printStmt
--     ]
--     where
--         parseAssignment = do
--             var <- identifier
--             reservedOp "="
--             expr <- parseExpr
--             return (Assignment var expr)
--         loopStmt = do
--             reservedOp "while"
--             cond <- parseExpr
--             reservedOp ":"
--             -- indent
--             statements <- many1 parseStmt
--             -- dedent
--             return (Loop cond statements)
--         ifStmt = do
--             reservedOp "if"
--             cond <- parseExpr
--             reservedOp ":"
--             -- indent
--             statements <- many1 parseStmt
--             -- dedent
--             return (If cond statements)
--         passStmt = do
--             reservedOp "pass"
--             return Pass
--         printStmt = do
--             reservedOp "print"
--             expr <- parens parseExpr
--             return (Print expr)

-- parsePetite :: Parser Program
-- parsePetite = do
--     whiteSpace
--     statements <- parseStmt `endBy` whiteSpace
--     return (Program statements)
