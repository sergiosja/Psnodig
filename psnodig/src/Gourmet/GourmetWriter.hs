module Gourmet.GourmetWriter (writeGourmet) where

import Control.Monad.Writer
import Syntax

type GourmetWriter = Writer String

-- Helper funcs

addIndents :: Int -> String
addIndents n = replicate n '\t'

-- Writer

writeExp :: Expression -> GourmetWriter ()
writeExp (Constant n) = tell $ show n
writeExp (VariableExp var) = tell var
writeExp (Boolean val) = case val of
    True -> tell "true"
    False -> tell "false"
writeExp (BinaryExp op exp1 exp2) = do
    writeExp exp1
    writeOp op
    writeExp exp2
writeExp (Call functioncall) = do
    writeFunctionCall functioncall
writeExp (Array entries) = do
    tell "["
    case length entries of
        0 -> tell "]"
        1 -> (writeExp $ head entries) >> tell "]"
        _ -> do
            mapM_ (\x -> (writeExp x) >> tell ", ") (init entries)
            (writeExp $ last entries) >> tell "]"
writeExp (ArrayExp name index) = do
    tell $ name ++ "["
    writeExp index
    tell "]"

writeFunctionCall :: FunctionCall -> GourmetWriter ()
writeFunctionCall (FunctionCall funcname args) = do
    tell $ funcname ++ "("
    case length args of
        0 -> tell ")"
        1 -> (writeExp $ head args) >> tell ")"
        _ -> do
            mapM_ (\arg -> (writeExp arg) >> tell ", ") (init args)
            (writeExp $ last args) >> tell ")"


writeStmt :: Statement -> Int -> GourmetWriter ()
writeStmt (Assignment var expr) _ = do
    tell var
    tell " := "
    writeExp expr
writeStmt (Loop expr stmts) indent = do
    tell "while "
    writeExp expr
    tell " {\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (If expr stmts) indent = do
    tell "if "
    writeExp expr
    tell " {\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt Pass _ = do
    tell "pass"
writeStmt (Return expr) _ = do
    tell "return "
    writeExp expr
writeStmt (Print expr) _ = do
    tell "fmt.Println("
    writeExp expr
    tell ")"

writeFunc :: Function -> GourmetWriter ()
writeFunc (Function funcname args stmts) = do
    tell $ "func " ++ funcname ++ "("
    case length args of
        0 -> tell ") {\n"
        1 -> tell $ head args ++ ") {\n"
        _ -> do
            mapM_ (\a -> (tell $ a ++ ", ")) (init args)
            tell $ last args ++ ") {\n"
    mapM_ (\stmt -> tell "\t" >> writeStmt stmt 1 >> tell "\n") stmts
    tell "}"

writeOp :: Operator -> GourmetWriter ()
writeOp Plus = tell " + "
writeOp Minus = tell " - "
writeOp Times = tell " * "
writeOp Division = tell " / "
writeOp LessThan = tell " < "
writeOp LessThanEqual = tell " <= "
writeOp GreaterThan = tell " > "
writeOp GreaterThanEqual = tell " >= "
writeOp Equal = tell " == "
writeOp NotEqual = tell " != "

writeGourmet :: Program -> GourmetWriter ()
writeGourmet (Program funcs funcCall) = do
    mapM_ (\f -> (writeFunc f) >> tell "\n\n") funcs
    writeFunctionCall funcCall