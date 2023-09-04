module Syntax where

data Program = Program [Statement]
    deriving (Eq, Show, Read)

data Statement =
      Assignment String Expression
    | Loop Expression [Statement]
    | If Expression [Statement]
    | Print Expression
    deriving (Eq, Show, Read)

data Expression =
      Constant Int
    | VariableExp String
    | BinaryExp Operator Expression Expression
    -- | Return Expression
    deriving (Eq, Show, Read)

data Operator =
      Plus
    | Minus 
    | Times 
    | Division 
    | LessThan 
    | GreaterThan
    | Equal
    | NotEqual
    deriving (Eq, Show, Read)

{-

* 0 or more
+ 1 or more

Program := Statement*
Statement := Assignment | Loop | Print
Expression := Constant | VariableExpression | BinaryExpression
Operator := + | - | * | / | < | > | == | !=

-}