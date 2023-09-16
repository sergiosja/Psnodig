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
    | Loop Expression [Statement] -- List cannot be empty!
    | If Expression [Statement] -- List cannot be empty!
    | Print Expression
    | Return Expression
    | Pass
    deriving (Eq, Show, Read)

data Expression =
      Constant Int
    | VariableExp String
    | BinaryExp Operator Expression Expression
    | Array [Expression] -- maybe it should just be ints
    | ArrayExp String Expression -- must look into this. f()[4] is valid if f -> Array. Should also be able to do k[1][2]
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
