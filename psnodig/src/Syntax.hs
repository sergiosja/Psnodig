{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Syntax where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- Program

data Program = Program (Maybe ProgramDescription) [StructDecl] [FunctionDecl] (Maybe FunctionCall)
    deriving (Eq, Show, Read, Ord)

data ProgramDescription = ProgramDescription String String
    deriving (Eq, Show, Read, Ord)

-- Structs

data StructDecl = StructDecl String [Argument]
    deriving (Eq, Show, Read, Ord)

data Struct = Struct String [Expression]
    deriving (Eq, Show, Read, Ord)

data StructField = StructField Expression Expression
    deriving (Eq, Show, Read, Ord)

-- Functions

data FunctionDecl = FunctionDecl String [Argument] [Statement] -- legg inn typer her
    deriving (Eq, Show, Read, Ord)

data FunctionCall = FunctionCall String [Expression]
    deriving (Eq, Show, Read, Ord)


data Argument = Argument String Type
    deriving (Eq, Show, Read, Ord)

-- Statements

data Statement =
      Assignment AssignmentVar Expression
    | Loop Expression [Statement]
    | If Expression [Statement] (Maybe Else)
    | ForEach String Expression [Statement] -- change expr to `data Iterable = ListIterable | .. | TextIterable`
    | For String Expression Expression [Statement]
    | CallStmt FunctionCall
    | Return Expression
    | HashStmt Statement
    | AnnotationStmt String [Statement]
    | Break
    | Continue
    deriving (Eq, Show, Read, Ord)

data AssignmentVar =
      VariableTarget String
    | ListIndexTarget String [Expression]
    | StructFieldTarget StructField
    deriving (Eq, Show, Read, Ord)

data Else =
      ElseIf Expression [Statement] (Maybe Else)
    | Else [Statement]
    deriving (Eq, Show, Read, Ord)

-- Expressions

data Expression =
      Constant Value
    | VariableExp String
    | BinaryExp Operator Expression Expression
    | ListIndex String [Expression]
    | CallExp FunctionCall
    | Not Expression
    | StructExpr Struct -- flytt denne ut og lag en assignmentValue likevel
    | StructFieldExp StructField
    -- | ParensExpr Expression -- parses med parenteser
    deriving (Eq, Show, Read, Ord)

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

-- Values

data Value =
      Nil
    | Boolean Bool
    | Number Integer
    | Decimal Double
    | Text String
    | List [Expression]
    | HashSet (Set.Set Expression)
    | HashMap (Map.Map Expression Expression)
    | StructVal [(String, Value)]
    deriving (Eq, Show, Read, Ord)

data Type =
      NilType
    | BooleanType
    | NumberType
    | DecimalType
    | TextType
    | ListType
    | HashSetType
    | HashMapType
    | StructType String -- i interpreten kan dette sjekkes opp mot den i structdecl!
    deriving (Eq, Show, Read, Ord)
