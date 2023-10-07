module Gourmet.GourmetWriter (writeGourmet) where

import Control.Monad.Writer
import Syntax

type GourmetWriter = Writer String

-- Helper funcs

addIndents :: Int -> String
addIndents n = replicate n '\t'

-- Writer

writeStruct :: Struct -> GourmetWriter ()
writeStruct (Struct name args) = do
    tell $ "struct " ++ name ++ " {"
    case length args of
        0 -> tell "}"
        1 -> tell $ "\n\t" ++ head (getArguments args) ++ "\n}"
        _ -> do
            tell "\n"
            mapM_ (\a -> (tell $ "\t" ++ a ++ ",\n")) (init (getArguments args))
            tell $ "\t" ++ last (getArguments args) ++ "\n}"

writeStructField :: StructField -> GourmetWriter ()
writeStructField (StructField struct field) =
    tell $ struct ++ "." ++ field

writeStructAssignment :: StructAssignment -> GourmetWriter ()
writeStructAssignment (StructAssignment struct args) = do
    tell $ struct ++ "("
    case length args of
        0 -> tell ")"
        1 -> do
            writeExp $ head args
            tell ")"
        _ -> do
            mapM_ (\a -> (writeExp a) >> tell ", ") (init args)
            (writeExp (last args)) >> tell ")"

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
    tell $ "not "
    writeExp expr
writeExp (StructFieldExp struct) =
    writeStructField struct

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
writeAssignmentTarget (ArrayIndexTarget var expr) = do
    tell $ var ++ "["
    writeExp expr
    tell "]"
writeAssignmentTarget (StructFieldTarget struct) =
    writeStructField struct

writeAssignmentValue :: AssignmentValue -> GourmetWriter ()
writeAssignmentValue (ExpressionValue expr) = writeExp expr
writeAssignmentValue (StructValue struct) = writeStructAssignment struct

writeStmt :: Statement -> Int -> GourmetWriter ()
writeStmt (Assignment target value) _ = do
    writeAssignmentTarget target
    tell " := "
    writeAssignmentValue value
writeStmt (Loop expr stmts) indent = do
    tell "while "
    writeExp expr
    tell " {\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (ForEach item expr stmts) indent = do
    tell $ "for " ++ item ++ " := "
    writeExp expr
    tell " {\n"
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
writeStmt Break _ =
    tell "break"
writeStmt Continue _ =
    tell "continue"

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

getArguments :: [Argument] -> [String]
getArguments = map writeArg
    where
        writeArg (ArrayArg name t) = name ++ " " ++ t ++ "*"
        writeArg (SingleArg name t) = name ++ " " ++ t

writeFunc :: Function -> GourmetWriter ()
writeFunc (Function funcname args stmts) = do
    tell $ "func " ++ funcname ++ "("
    case length args of
        0 -> tell ") {\n"
        1 -> tell $ head (getArguments args) ++ ") {\n"
        _ -> do
            mapM_ (\a -> (tell $ a ++ ", ")) (init (getArguments args))
            tell $ last (getArguments args) ++ ") {\n"
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
writeOp Modulo = tell " % "

writeGourmet :: Program -> GourmetWriter ()
writeGourmet (Program structs funcs funcCall) = do
    mapM_ (\s -> (writeStruct s) >> tell "\n\n") structs
    mapM_ (\f -> (writeFunc f) >> tell "\n\n") funcs
    writeFunctionCall funcCall