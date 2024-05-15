module Gourmet.GourmetWriter
    ( writeGourmet
    , writeValue
    , writeProgramDescription
    , writeExpr
    , writeStmt
    , writeStructDecl
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Writer
import Syntax

type GourmetWriter = Writer String

-- Helper funcs

addIndents :: Int -> String
addIndents n = replicate n '\t'

writeArgs :: [Expression] -> String -> GourmetWriter ()
writeArgs [] b = tell b
writeArgs [x] b = writeExpr x >> tell b
writeArgs (x:y:ys) b = writeExpr x >> tell ", " >> writeArgs (y:ys) b

-- Writer

writeStructDecl :: StructDecl -> GourmetWriter ()
writeStructDecl (StructDecl name args) = do
    tell $ "struct " ++ name ++ " {"
    case length args of
        0 -> tell "}"
        1 -> tell $ "\n\t" ++ head (getArguments args) ++ "\n}"
        _ -> do
            tell "\n"
            mapM_ (\a -> (tell $ "\t" ++ a ++ ",\n")) (init (getArguments args))
            tell $ "\t" ++ last (getArguments args) ++ "\n}"

writeStructField :: StructField -> GourmetWriter ()
writeStructField (StructField expr1 expr2) =
   writeExpr expr1 >> tell "." >> writeExpr expr2

writeStruct :: Struct -> GourmetWriter ()
writeStruct (Struct name args) = do
    tell $ "struct " ++ name ++ "("
    writeArgs args ")"

-- Values

writeValue :: Value -> GourmetWriter ()
writeValue (Nil) = tell "nil"
writeValue (Boolean bool) =
    tell $ if bool == True then "true" else "false"
writeValue (Number n) = tell $ show n
writeValue (Decimal d) = tell $ show d
writeValue (Text str) = tell $ "\"" ++ str ++ "\""
writeValue (List exprs) = do
    tell "["
    writeArgs exprs "]"
writeValue (HashSet exprs) = do
    tell "set{"
    writeArgs (Set.toList exprs) "}"
writeValue (HashMap hmap) = do
    tell "map{"
    let pairs = Map.toList hmap
    case length pairs of
        0 -> tell "}"
        1 -> (writePair $ head pairs) >> tell "}"
        _ -> do
            mapM_ (\p -> (writePair p) >> tell ", ") (init pairs)
            (writePair $ last pairs) >> tell "}"
writeValue (StructVal _) = undefined -- these aren't parsed

writePair :: (Expression, Expression) -> GourmetWriter ()
writePair (x, y) = writeExpr x >> tell ": " >> writeExpr y

-- Expressions

writeExpr :: Expression -> GourmetWriter ()
writeExpr (Constant v) = writeValue v
writeExpr (VariableExp var) = tell var
writeExpr (BinaryExp op exp1 exp2) = do
    writeExpr exp1
    writeOp op
    writeExpr exp2
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

writeFunctionCall :: FunctionCall -> GourmetWriter ()
writeFunctionCall (FunctionCall funcname args) = do
    tell $ funcname ++ "("
    writeArgs args ")"

writeAssignmentTarget :: AssignmentTarget -> GourmetWriter ()
writeAssignmentTarget (VariableTarget var) = tell var
writeAssignmentTarget (ListIndexTarget var indexes) = do
    tell var
    mapM_ (\x -> tell "[" >> writeExpr x >> tell "]") indexes
writeAssignmentTarget (StructFieldTarget struct) =
    writeStructField struct

writeAssignmentValue :: AssignmentValue -> GourmetWriter ()
writeAssignmentValue (ExpressionValue expr) = writeExpr expr
writeAssignmentValue (StructValue struct) = writeStruct struct

writeStmt :: Statement -> Int -> GourmetWriter ()
writeStmt (Assignment target value) _ = do
    writeAssignmentTarget target
    tell " := "
    writeAssignmentValue value
writeStmt (Loop expr stmts) indent = do
    tell "while "
    writeExpr expr
    tell " {\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (ForEach item expr stmts) indent = do
    tell $ "for " ++ item ++ " := "
    writeExpr expr
    tell " {\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (For item from to stmts) indent = do
    tell $ "for " ++ item ++ " := "
    writeExpr from
    tell ", "
    writeExpr to
    tell " {\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (If expr stmts maybeElse) indent = do
    tell "if "
    writeExpr expr
    tell " {\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
    case maybeElse of
        Just elsePart -> writeElse elsePart indent
        Nothing -> return ()
writeStmt (Return expr) _ = do
    tell "return "
    writeExpr expr
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
    tell " else if " >> writeExpr expr >> tell " {\n"
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
getArguments = map (\(Argument name t) -> name ++ " " ++ t)

writeFunc :: FunctionDecl -> GourmetWriter ()
writeFunc (FunctionDecl funcname args stmts) = do
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
writeOp And = tell " && "
writeOp Or = tell " || "
writeOp Modulo = tell " % "

writeProgramDescription :: Maybe ProgramDescription -> GourmetWriter ()
writeProgramDescription Nothing = return ()
writeProgramDescription (Just (ProgramDescription input output)) = do
    tell $ "? " ++ input ++ " ?\n"
    tell $ "! " ++ output ++ " !\n\n"

writeGourmet :: Program -> GourmetWriter ()
writeGourmet (Program programDescription structs funcs funcCall) = do
    writeProgramDescription programDescription
    mapM_ (\s -> (writeStructDecl s) >> tell "\n\n") structs
    mapM_ (\f -> (writeFunc f) >> tell "\n\n") funcs
    case funcCall of
        Just f -> writeFunctionCall f
        Nothing -> return ()