module Pytite.PytiteWriter (writePytite) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Writer
import Syntax

type PytiteWriter = Writer String

-- Helper funcs

addIndents :: Int -> String
addIndents n = replicate n '\t'

intercalateExprs :: [Expression] -> String -> PytiteWriter ()
intercalateExprs [] b = tell b
intercalateExprs [x] b = writeExp x >> tell b
intercalateExprs (x:y:ys) b = writeExp x >> tell ", " >> intercalateExprs (y:ys) b

intercalateArgs :: [String] -> String
intercalateArgs [] = "):\n"
intercalateArgs [x] = x ++ "):\n"
intercalateArgs (x:xs) = x ++ ", " ++ intercalateArgs xs

unwrapArgs :: [Argument] -> [String]
unwrapArgs = map (\(Argument name _) -> name)

translateArg :: Argument -> String
translateArg (Argument name t) = name ++
    case t of
        "int" -> ": int"
        "float" -> ": float"
        "double" -> ": float"
        "str" -> ": str"
        "boolean" -> ": bool"
        "nil" -> ": None"
        _ -> ""

translateArgs :: [Argument] -> [String]
translateArgs args =
    map translateArg args

-- Writer

writeClass :: StructDecl -> PytiteWriter ()
writeClass (StructDecl name args) = do
    tell $ "class " ++ name ++ ":\n"
    tell $ "\tdef __init__(self" ++ writeInstanceVariables (unwrapArgs args) ++ "):\n"
    mapM_ tell $ map (\x -> "\t\tself." ++ x ++ " = " ++ x ++ "\n") (unwrapArgs args)
    where
        writeInstanceVariables :: [String] -> String
        writeInstanceVariables [] = ""
        writeInstanceVariables [x] = ", " ++ x
        writeInstanceVariables (x:xs) = ", " ++ x ++ writeInstanceVariables xs

writeStructField :: StructField -> PytiteWriter ()
writeStructField (StructField expr1 expr2) =
   writeExp expr1 >> tell "." >> writeExp expr2

writeStruct :: Struct -> PytiteWriter ()
writeStruct (Struct name args) = do
    tell $ name ++ "("
    intercalateExprs args ")"


-- Values. fiks sånn at strenger i en map etc forblir det!

writeValue :: Value -> PytiteWriter ()
writeValue Nil = tell "None"
writeValue (Boolean bool) =
    tell $ if bool == True then "True" else "False"
writeValue (Number n) = tell $ show n
writeValue (Decimal d) = tell $ show d
writeValue (Text str) = tell $ show str
writeValue (List exprs) = do
    tell "["
    intercalateExprs exprs "]"
writeValue (HashSet exprs) = do
    tell "{"
    intercalateExprs (Set.toList exprs) ")"
writeValue (HashMap hmap) = do
    tell "{"
    let pairs = Map.toList hmap
    case length pairs of
        0 -> tell "}"
        1 -> (writePair $ head pairs) >> tell "}"
        _ -> do
            mapM_ (\p -> (writePair p) >> tell ", ") (init pairs)
            (writePair $ last pairs) >> tell "}"
writeValue (StructVal _) = undefined -- these aren't parsed

writePair :: (Expression, Expression) -> PytiteWriter ()
writePair (x, y) = writeExp x >> tell ": " >> writeExp y

-- Expressions

writeExp :: Expression -> PytiteWriter ()
writeExp (Constant v) = writeValue v
writeExp (VariableExp var) = tell var
writeExp (BinaryExp op exp1 exp2) =
    writeExp exp1 >> writeOp op >> writeExp exp2
writeExp (CallExp functioncall) = do
    writeFunctionCall functioncall
writeExp (ListIndex name indexes) = do
    tell name
    mapM_ (\x -> tell "[" >> writeExp x >> tell "]") indexes
writeExp (Not expr) = do
    tell $ "not "
    writeExp expr
writeExp (StructExpr struct) =
    writeStruct struct
writeExp (StructFieldExp structField) =
    writeStructField structField

writeFunctionCall :: FunctionCall -> PytiteWriter ()
writeFunctionCall (FunctionCall funcname args) = do
    tell $ funcname ++ "("
    intercalateExprs args ")"

writeAssignmentTarget :: AssignmentTarget -> PytiteWriter ()
writeAssignmentTarget (VariableTarget var) = tell var
writeAssignmentTarget (ListIndexTarget var indexes) = do
    tell var
    mapM_ (\x -> tell "[" >> writeExp x >> tell "]") indexes >> tell " "
writeAssignmentTarget (StructFieldTarget struct) =
    writeStructField struct

writeAssignmentValue :: AssignmentValue -> PytiteWriter ()
writeAssignmentValue (ExpressionValue expr) = writeExp expr
writeAssignmentValue (StructValue struct) = writeStruct struct

writeStmt :: Statement -> Int -> PytiteWriter ()
writeStmt (Assignment target value) _ = do
    writeAssignmentTarget target
    tell " = "
    writeAssignmentValue value
writeStmt (Loop expr stmts) indent = do
    tell "while "
    writeExp expr
    tell ":\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
writeStmt (ForEach item expr stmts) indent = do
    tell $ "for " ++ item ++ " in "
    writeExp expr
    tell ":\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
writeStmt (For item from to stmts) indent = do
    tell $ "for " ++ item ++ " in range("
    writeExp from
    tell ", "
    writeExp to
    tell "):\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
writeStmt (If expr stmts maybeElse) indent = do
    tell "if "
    writeExp expr
    tell ":\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    case maybeElse of
        Just elsePart -> writeElse elsePart indent
        Nothing -> return ()
writeStmt (Return expr) _ = do
    tell "return "
    writeExp expr
writeStmt (CallStmt functioncall) _ =
    writeFunctionCall functioncall
writeStmt Break _ =
    tell "break"
writeStmt Continue _ =
    tell "continue"
writeStmt (HashStmt _) _ = undefined
writeStmt (AnnotationStmt _ _) _ = undefined


writeElse :: Else -> Int -> PytiteWriter ()
writeElse (ElseIf expr stmts maybeElse) indent = do
    tell "elif " >> writeExp expr >> tell ":\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    case maybeElse of
        Just elsePart -> writeElse elsePart indent
        Nothing -> return ()
writeElse (Else stmts) indent = do
    tell "else:\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts

writeFunc :: Function -> PytiteWriter ()
writeFunc (Function funcname args stmts) = do
    tell $ "def " ++ funcname ++ "(" ++ (intercalateArgs $ translateArgs args)
    mapM_ (\stmt -> tell "\t" >> writeStmt stmt 1 >> tell "\n") stmts

writeOp :: Operator -> PytiteWriter ()
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

writePytite :: Program -> PytiteWriter ()
writePytite (Program _ structs funcs funcCall) = do
    mapM_ (\s -> (writeClass s) >> tell "\n\n") structs
    mapM_ (\f -> (writeFunc f) >> tell "\n\n") funcs
    case funcCall of
        Just f -> writeFunctionCall f
        Nothing -> return ()