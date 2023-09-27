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
writeExp (CallExp functioncall) = do
    writeFunctionCall functioncall
writeExp (ArrayExp array) =
    writeArray array
writeExp (ArrayIndex name index) = do
    tell $ name ++ "["
    writeExp index
    tell "]"
writeExp (Not expr) = do
    tell $ "!"
    writeExp expr

writeArray :: Array -> GourmetWriter ()
writeArray (Array entries) = do
    tell "["
    case length entries of
        0 -> tell "]"
        1 -> (writeExp $ head entries) >> tell "]"
        _ -> do
            mapM_ (\x -> (writeExp x) >> tell ", ") (init entries)
            (writeExp $ last entries) >> tell "]"

writeFunctionCall :: FunctionCall -> GourmetWriter ()
writeFunctionCall (FunctionCall funcname args) = do
    tell $ funcname ++ "("
    case length args of
        0 -> tell ")"
        1 -> (writeExp $ head args) >> tell ")"
        _ -> do
            mapM_ (\arg -> (writeExp arg) >> tell ", ") (init args)
            (writeExp $ last args) >> tell ")"

writeAssignmentTarget :: AssignmentTarget -> GourmetWriter ()
writeAssignmentTarget (VariableTarget var) = tell var
writeAssignmentTarget (ArrayIndexTarget var expr) = do -- fix this when we collect arrays globally
    tell $ var ++ "["
    writeExp expr
    tell "]"

writeStmt :: Statement -> Int -> GourmetWriter ()
writeStmt (Assignment target expr) _ = do
    writeAssignmentTarget target
    tell " := "
    writeExp expr
writeStmt (Loop expr stmts) indent = do
    tell "while "
    writeExp expr
    tell " {\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (ForEach item array stmts) indent = do
    tell $ "for " ++ item ++ " := " ++ array ++ " {\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (For item from to stmts) indent = do
    tell $ "for " ++ item ++ " := "
    writeExp from
    tell ", "
    writeExp to
    tell "{\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (If expr stmts maybeElse) indent = do
    tell "if "
    writeExp expr
    tell " {\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
    case maybeElse of
        Just elsePart -> writeElse elsePart indent
        Nothing -> return ()
writeStmt Pass _ = do
    tell "pass"
writeStmt (Return expr) _ = do
    tell "return "
    writeExp expr
writeStmt (CallStmt functioncall) _ = do
    writeFunctionCall functioncall
writeStmt (HashStmt stmt) indent = do
    tell "# "
    writeStmt stmt indent
writeStmt (AnnotationStmt description stmts) indent = do
    tell $ "@{" ++ description ++ "}{\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"



writeElse :: Else -> Int -> GourmetWriter ()
writeElse (ElseIf expr stmts maybeElse) indent = do
    tell " else if " >> writeExp expr >> tell " {\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
    case maybeElse of
        Just elsePart -> writeElse elsePart indent
        Nothing -> return ()
writeElse (Else stmts) indent = do
    tell " else {\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"

getFuncArgs :: [FunctionArg] -> [String]
getFuncArgs = map getArgName
    where
        getArgName (ArrayArg name _) = name
        getArgName (IntArg name _) = name

writeFunc :: Function -> GourmetWriter ()
writeFunc (Function funcname args stmts) = do
    tell $ "func " ++ funcname ++ "("
    case length args of
        0 -> tell ") {\n"
        1 -> tell $ head (getFuncArgs args) ++ ") {\n"
        _ -> do
            mapM_ (\a -> (tell $ a ++ ", ")) (init (getFuncArgs args))
            tell $ last (getFuncArgs args) ++ ") {\n"
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
writeOp And = tell " and "
writeOp Or = tell " or "

writeGourmet :: Program -> GourmetWriter ()
writeGourmet (Program funcs funcCall) = do
    mapM_ (\f -> (writeFunc f) >> tell "\n\n") funcs
    writeFunctionCall funcCall