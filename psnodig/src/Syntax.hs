{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Syntax where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- Program

data Program = Program [StructDecl] [Function] FunctionCall
    deriving (Eq, Show, Read, Ord)

data Argument = Argument String String
    deriving (Eq, Show, Read, Ord)

data Value =
      Nil
    | Boolean Bool
    | Number Integer
    | Text String
    | List [Expression]
    | HashSet (Set.Set Expression)
    | HashMap (Map.Map Expression Expression)
    | StructVal Struct
    deriving (Eq, Show, Read, Ord)

-- Struct related

data StructDecl = StructDecl String [Argument]
    deriving (Eq, Show, Read, Ord)

data StructField = StructField Expression Expression
    deriving (Eq, Show, Read, Ord)

data Struct = Struct String [Expression]
    deriving (Eq, Show, Read, Ord)

-- Function related

data Function = Function String [Argument] [Statement]
    deriving (Eq, Show, Read, Ord)

data FunctionCall = FunctionCall String [Expression]
    deriving (Eq, Show, Read, Ord)

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
    deriving (Eq, Show, Read, Ord)

data AssignmentTarget =
      VariableTarget String             -- x =
    | ListIndexTarget String [Expression] -- x[0] =
    | StructFieldTarget StructField     -- x.left =
    deriving (Eq, Show, Read, Ord)

data AssignmentValue =
      ExpressionValue Expression    -- = 5 + 5
    | StructValue Struct  -- = struct Person
    deriving (Eq, Show, Read, Ord)

data Else =
      ElseIf Expression [Statement] (Maybe Else)
    | Else [Statement]
    deriving (Eq, Show, Read, Ord)

data Expression =
      Constant Value
    | VariableExp String
    | BinaryExp Operator Expression Expression
    | ListIndex String [Expression]
    | CallExp FunctionCall
    | Not Expression -- fix this. "Not" should only be possible with boolean!! eller? meh, bare ha bval og gjør sjekker:)
    | StructExpr Struct
    | StructFieldExp StructField
    deriving (Eq, Show, Read, Ord)

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
    deriving (Eq, Show, Read, Ord)
