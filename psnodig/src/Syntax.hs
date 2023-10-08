{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Syntax where

-- Program

data Program = Program [Struct] [Function] FunctionCall
    deriving (Eq, Show, Read)

data Argument =
      SingleArg String String
    | ArrayArg String String
    deriving (Eq, Show, Read)

-- data Value =
--       Nil
--     | Boolean Bool
--     | Number Integer
--     | Text String
--     | List [Value]

-- Struct related

data Struct = Struct String [Argument]
    deriving (Eq, Show, Read)

data StructField = StructField String String
    deriving (Eq, Show, Read)

data StructAssignment = StructAssignment String [Expression]
    deriving (Eq, Show, Read)

-- Function related

data Function = Function String [Argument] [Statement]
    deriving (Eq, Show, Read)

data FunctionCall = FunctionCall String [Expression]
    deriving (Eq, Show, Read)

-- Collections

data Array =
      FullArray [Expression]
    | EmptyArray ArrayDecl
    deriving (Eq, Show, Read)

data ArrayDecl =
      BaseType String
    | ArrayType Expression ArrayDecl
    deriving (Eq, Show, Read)

data HashMap = HashMap String String
    deriving (Eq, Show, Read)

data HashSet = HashSet String
    deriving (Eq, Show, Read)

-- Statements and expressions

data Statement =
      Assignment AssignmentTarget AssignmentValue
    | Loop Expression [Statement]
    | If Expression [Statement] (Maybe Else)
    | ForEach String Expression [Statement]
    | For String Expression Expression [Statement]
    | CallStmt FunctionCall
    | Return Expression
    | HashStmt Statement
    | AnnotationStmt String [Statement]
    | Break
    | Continue
    deriving (Eq, Show, Read)

data AssignmentTarget =
      VariableTarget String
    | ArrayIndexTarget String Expression
    | StructFieldTarget StructField
    deriving (Eq, Show, Read)

data AssignmentValue =
      ExpressionValue Expression
    | StructValue StructAssignment
    | HashMapValue HashMap
    | HashSetValue HashSet
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
    | StructFieldExp StructField
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
    | Modulo
    deriving (Eq, Show, Read)
