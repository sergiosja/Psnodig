module Petite.PetiteTranspiler (transpilePetite) where

import Control.Monad.Writer
import Syntax

type PetiteTranspiler = Writer String

transpileExp :: Expression -> PetiteTranspiler ()
transpileExp (Constant n) = tell $ show n
transpileExp (VariableExp var) = tell var
transpileExp (BinaryExp op exp1 exp2) = do
    transpileExp exp1
    transpileOp op
    transpileExp exp2

transpileStmt :: Statement -> PetiteTranspiler ()
transpileStmt (Assignment var expr) = do
    tell var
    tell " = "
    transpileExp expr
transpileStmt (Loop expr stmts) = do
    tell "while "
    transpileExp expr
    tell ":\n"
    mapM_ (\stmt -> tell "\t" >> transpileStmt stmt >> tell "\n") $ init stmts
    tell "\t"
    transpileStmt $ last stmts
transpileStmt (If expr stmts) = do
    tell "if "
    transpileExp expr
    tell ":\n"
    mapM_ (\stmt -> tell "\t" >> transpileStmt stmt >> tell "\n") $ init stmts
    tell "\t"
    transpileStmt $ last stmts
transpileStmt (Print expr) = do
    tell "print("
    transpileExp expr
    tell ")"

transpileOp :: Operator -> PetiteTranspiler ()
transpileOp Plus = tell " + "
transpileOp Minus = tell " - "
transpileOp Times = tell " * "
transpileOp Division = tell " / "
transpileOp LessThan = tell " < "
transpileOp GreaterThan = tell " > "
transpileOp Equal = tell " == "
transpileOp NotEqual = tell " != "


transpilePetite :: Program -> PetiteTranspiler ()
transpilePetite (Program stmts) =
    mapM_ (\stmt -> transpileStmt stmt >> tell "\n") stmts