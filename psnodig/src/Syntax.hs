{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Syntax where

-- Program

data Program = Program [Function] FunctionCall
    deriving (Eq, Show, Read)

data Function = Function String [String] [Statement]
    deriving (Eq, Show, Read)

data FunctionCall = FunctionCall String [Expression]
    deriving (Eq, Show, Read)

-- Statements and expressions

data Statement =
      Assignment String Expression
    | Loop Expression [Statement]
    | If Expression [Statement] (Maybe Else)
    | Print Expression
    | Return Expression
    | Pass
    deriving (Eq, Show, Read)

data Else =
      ElseIf Expression [Statement] (Maybe Else)
    | Else [Statement]
    deriving (Eq, Show, Read)

data Expression =
      Constant Int
    | VariableExp String
    | BinaryExp Operator Expression Expression
    | Array [Expression]
    | ArrayExp String Expression
    -- FIXME:
    -- f()[4] is valid if f -> Array. Suggestion: Make Array it's own data type. Consequence: Refactor assignments
    -- Should also be able to do k[1][2][3]etc. Suggestion: String Expression (Maybe [Expression])
    | Boolean Bool
    | Call FunctionCall
    deriving (Eq, Show, Read)

-- Operators

data Operator =
      Plus
    | Minus
    | Times
    | Division
    | LessThan
    | LessThanEqual
    | GreaterThan
    | GreaterThanEqual
    | Equal
    | NotEqual
    deriving (Eq, Show, Read)
