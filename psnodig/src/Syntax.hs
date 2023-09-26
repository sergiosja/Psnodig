{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Syntax where

-- Program

data Program = Program [Function] FunctionCall
    deriving (Eq, Show, Read)

-- Function related 

data Function = Function String [FunctionArg] [Statement]
    deriving (Eq, Show, Read)

data FunctionArg = IntArg String String | ArrayArg String String
    deriving (Eq, Show, Read)

data FunctionCall = FunctionCall String [Expression]
    deriving (Eq, Show, Read)

-- Array

data Array = Array [Expression]
    deriving (Eq, Show, Read)

-- Statements and expressions

data Statement =
      Assignment AssignmentTarget Expression
    | Loop Expression [Statement]
    | If Expression [Statement] (Maybe Else)
    | ForEach String String [Statement] -- (Either String Array) can also work in the future. must study more code
    | For String Expression Expression [Statement] -- if I need to add step: (Maybe Expression).
    | CallStmt FunctionCall
    | Return Expression
    | Pass
    deriving (Eq, Show, Read)

data AssignmentTarget = VariableTarget String | ArrayIndexTarget String Expression
    deriving (Eq, Show, Read)

data Else = ElseIf Expression [Statement] (Maybe Else) | Else [Statement]
    deriving (Eq, Show, Read)

data Expression =
      Constant Int
    | VariableExp String
    | BinaryExp Operator Expression Expression
    | ArrayExp Array
    | ArrayIndex String Expression
    -- TODO:
    -- f()[4] should be valid valid if f -> Array. Suggestion: Make Array it's own data type. Consequence: Refactor assignments
    -- Should also be able to do k[1][2][3]etc. Suggestion: String Expression (Maybe [Expression])
    -- although, when is f()[4] ever necessary when writing algorithms? must study more algorithms!!
    | Boolean Bool
    | CallExp FunctionCall
    | Not Expression
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
    | And
    | Or
    deriving (Eq, Show, Read)
