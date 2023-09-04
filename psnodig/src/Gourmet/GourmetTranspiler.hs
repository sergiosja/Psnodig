module Gourmet.GourmetTranspiler (transpileGourmet) where

import Control.Monad.Writer
import Syntax

-- The monad. Simple writer should do
type GourmetTranspiler = Writer String

transpileExp :: Expression -> GourmetTranspiler ()
transpileExp (Constant n) = tell $ show n
transpileExp (VariableExp var) = tell var
transpileExp (BinaryExp op exp1 exp2) = do
    transpileExp exp1
    transpileOp op
    transpileExp exp2

transpileStmt :: Statement -> GourmetTranspiler ()
transpileStmt (Assignment var expr) = do
    tell var
    tell " := "
    transpileExp expr
transpileStmt (Loop expr stmts) = do
    tell "while "
    transpileExp expr
    tell " { "
    mapM_ transpileStmt stmts
    tell " }"
transpileStmt (Print expr) = do
    tell "fmt.Println("
    transpileExp expr
    tell ")"

transpileOp :: Operator -> GourmetTranspiler ()
transpileOp Plus = tell " + "
transpileOp Minus = tell " - "
transpileOp Times = tell " * "
transpileOp Division = tell " / "
transpileOp LessThan = tell " < "
transpileOp GreaterThan = tell " > "
transpileOp Equal = tell " == "
transpileOp NotEqual = tell " != "


transpileGourmet :: Program -> GourmetTranspiler ()
transpileGourmet (Program stmts) =
    mapM_ transpileStmt stmts