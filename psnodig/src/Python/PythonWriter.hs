module Python.PythonWriter
    ( writeValue
    , writeProgramDescription
    , writeExpr
    , writeStmt
    , writeClass
    , writePython
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Writer
import Syntax

type PythonWriter = Writer String

-- Helper funcs

addIndents :: Int -> String
addIndents n = replicate n '\t'

intercalateExprs :: [Expression] -> String -> PythonWriter ()
intercalateExprs [] b = tell b
intercalateExprs [x] b = writeExpr x >> tell b
intercalateExprs (x:y:ys) b = writeExpr x >> tell ", " >> intercalateExprs (y:ys) b

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

writeClass :: StructDecl -> PythonWriter ()
writeClass (StructDecl name args) = do
    tell $ "class " ++ name ++ ":\n"
    tell $ "\tdef __init__(self" ++ writeInstanceVariables (unwrapArgs args) ++ "):\n"
    if null args then tell "\t\tpass"
    else mapM_ tell $ map (\x -> "\t\tself." ++ x ++ " = " ++ x ++ "\n") (unwrapArgs args)
    where
        writeInstanceVariables :: [String] -> String
        writeInstanceVariables [] = ""
        writeInstanceVariables [x] = ", " ++ x
        writeInstanceVariables (x:xs) = ", " ++ x ++ writeInstanceVariables xs

writeStructField :: StructField -> PythonWriter ()
writeStructField (StructField expr1 expr2) =
   writeExpr expr1 >> tell "." >> writeExpr expr2

writeStruct :: Struct -> PythonWriter ()
writeStruct (Struct name args) = do
    tell $ name ++ "("
    intercalateExprs args ")"


-- Values. fiks sånn at strenger i en map etc forblir det!

writeValue :: Value -> PythonWriter ()
writeValue Nil = tell "None"
writeValue (Boolean bool) =
    tell $ if bool == True then "True" else "False"
writeValue (Number n) = tell $ show n
writeValue (Decimal d) = tell $ show d
writeValue (Text str) = tell $ show str
writeValue (List exprs) = do
    tell "["
    intercalateExprs exprs "]"
writeValue (HashSet exprs) =
    if null exprs then tell "set()"
    else do
        tell "{"
        intercalateExprs (Set.toList exprs) "}"
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

writePair :: (Expression, Expression) -> PythonWriter ()
writePair (x, y) = writeExpr x >> tell ": " >> writeExpr y

-- Expressions

writeExpr :: Expression -> PythonWriter ()
writeExpr (Constant v) = writeValue v
writeExpr (VariableExp var) = tell var
writeExpr (BinaryExp op exp1 exp2) =
    writeExpr exp1 >> writeOp op >> writeExpr exp2
writeExpr (CallExp functioncall) = do
    writeFunctionCall functioncall
writeExpr (ListIndex name indexes) = do
    tell name
    mapM_ (\x -> tell "[" >> writeExpr x >> tell "]") indexes
writeExpr (Not expr) = do
    tell $ "not "
    writeExpr expr
writeExpr (StructExpr struct) =
    writeStruct struct
writeExpr (StructFieldExp structField) =
    writeStructField structField

writeAssignmentTarget :: AssignmentTarget -> PythonWriter ()
writeAssignmentTarget (VariableTarget var) = tell var
writeAssignmentTarget (ListIndexTarget var indexes) = do
    tell var
    mapM_ (\x -> tell "[" >> writeExpr x >> tell "]") indexes
writeAssignmentTarget (StructFieldTarget struct) =
    writeStructField struct

writeAssignmentValue :: AssignmentValue -> PythonWriter ()
writeAssignmentValue (ExpressionValue expr) = writeExpr expr
writeAssignmentValue (StructValue struct) = writeStruct struct

writeStmt :: Statement -> Int -> PythonWriter ()
writeStmt (Assignment target value) _ = do
    writeAssignmentTarget target
    tell " = "
    writeAssignmentValue value
    tell "\n"
writeStmt (Loop expr stmts) indent = do
    tell "while "
    writeExpr expr
    tell ":\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1)) stmts
writeStmt (ForEach item expr stmts) indent = do
    tell $ "for " ++ item ++ " in "
    writeExpr expr
    tell ":\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1)) stmts
writeStmt (For item from to stmts) indent = do
    tell $ "for " ++ item ++ " in range("
    writeExpr from
    tell ", "
    writeExpr to
    tell "):\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1)) stmts
writeStmt (If expr stmts maybeElse) indent = do
    tell "if "
    writeExpr expr
    tell ":\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1)) stmts
    case maybeElse of
        Just elsePart -> writeElse elsePart indent
        Nothing -> return ()
writeStmt (Return expr) _ = do
    tell "return "
    writeExpr expr
    tell "\n"
writeStmt (CallStmt functioncall) _ = do
    writeFunctionCall functioncall
    tell "\n"
writeStmt Break _ =
    tell "break\n"
writeStmt Continue _ =
    tell "continue\n"
writeStmt (HashStmt stmt) indent =
    writeStmt stmt indent
writeStmt as@(AnnotationStmt _ _) indent =
    writeAnnotationStmt as indent

writeAnnotationStmt :: Statement -> Int -> PythonWriter ()
writeAnnotationStmt (AnnotationStmt _ []) _ = return ()
writeAnnotationStmt (AnnotationStmt _ (x:xs)) indent = do
    writeStmt x indent
    mapM_ (\stmt -> (tell $ addIndents $ indent) >> writeStmt stmt indent) xs
writeAnnotationStmt _ _ = return ()

writeElse :: Else -> Int -> PythonWriter ()
writeElse (ElseIf expr stmts maybeElse) indent = do
    tell ((addIndents indent) ++ "elif ") >> writeExpr expr >> tell ":\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1)) stmts
    case maybeElse of
        Just elsePart -> writeElse elsePart indent
        Nothing -> return ()
writeElse (Else stmts) indent = do
    tell ((addIndents indent) ++ "else:\n")
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1)) stmts

writeFunc :: FunctionDecl -> PythonWriter ()
writeFunc (FunctionDecl funcname args stmts) = do
    tell $ "def " ++ funcname ++ "(" ++ (intercalateArgs $ translateArgs args)
    mapM_ (\stmt -> tell "\t" >> writeStmt stmt 1) stmts

writeFunctionCall :: FunctionCall -> PythonWriter ()
writeFunctionCall (FunctionCall "length" args) =
    writeFunctionCall (FunctionCall "len" args)
writeFunctionCall (FunctionCall "toString" args) =
    writeFunctionCall (FunctionCall "str" args)

writeFunctionCall (FunctionCall "add" args) =
    case length args of
        2 -> do
            writeExpr . head . tail $ args
            tell ".add("
            writeExpr $ head args
            tell ")"
        3 -> do
            writeExpr . head . tail . tail $ args
            tell "["
            writeExpr . head . tail $ args
            tell "] = "
            writeExpr $ head args
        _ -> do
            tell $ "add("
            intercalateExprs args ")"

writeFunctionCall (FunctionCall "get" args) =
    case length args of
        2 -> do
            writeExpr . head . tail $ args
            tell "["
            writeExpr $ head args
            tell "]"
        _ -> do
            tell $ "get("
            intercalateExprs args ")"

writeFunctionCall (FunctionCall "in" args) =
    case length args of
        2 -> do
            writeExpr $ head args
            tell " in "
            writeExpr . head . tail $ args
        _ -> do
            tell $ "in("
            intercalateExprs args ")"

writeFunctionCall (FunctionCall "pop" args) =
    case length args of
        1 -> do
            writeExpr $ head args
            tell ".pop()"
        _ -> do
            tell $ "pop("
            intercalateExprs args ")"

writeFunctionCall (FunctionCall "append" args) =
    case length args of
        2 -> do
            writeExpr . head . tail $ args
            tell ".append("
            writeExpr $ head args
            tell ")"
        _ -> do
            tell $ "append("
            intercalateExprs args ")"

writeFunctionCall (FunctionCall funcname args) = do
    tell $ funcname ++ "("
    intercalateExprs args ")"

writeOp :: Operator -> PythonWriter ()
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


writeProgramDescription :: Maybe ProgramDescription -> PythonWriter ()
writeProgramDescription Nothing = return ()
writeProgramDescription (Just (ProgramDescription input output)) = do
    tell $ "# Input: " ++ input ++ "\n"
    tell $ "# Output: " ++ output ++ "\n\n"

writePython :: Program -> PythonWriter ()
writePython (Program programDescription structs funcs funcCall) = do
    writeProgramDescription programDescription
    mapM_ (\s -> (writeClass s) >> tell "\n") structs
    mapM_ (\f -> (writeFunc f) >> tell "\n") funcs
    case funcCall of
        Just f -> writeFunctionCall f
        Nothing -> return ()
