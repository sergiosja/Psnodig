{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Syntax where

-- Program

data Program = Program [Function] FunctionCall
    deriving (Eq, Show, Read)

-- Function related 

data Function = Function String [FunctionArg] [Statement]
    deriving (Eq, Show, Read)

data FunctionArg =
      IntArg String String
    | ArrayArg String String
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
    | ForEach String String [Statement]
    | For String Expression Expression [Statement]
    | CallStmt FunctionCall
    | Return Expression
    | HashStmt Statement
    | AnnotationStmt String [Statement]
    deriving (Eq, Show, Read)

data AssignmentTarget =
      VariableTarget String
    | ArrayIndexTarget String Expression
    deriving (Eq, Show, Read)

data Else =
      ElseIf Expression [Statement] (Maybe Else)
    | Else [Statement]
    deriving (Eq, Show, Read)

data Expression =
      Constant Int
    | VariableExp String
    | BinaryExp Operator Expression Expression
    | ArrayExp Array
    | ArrayIndex String Expression
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
