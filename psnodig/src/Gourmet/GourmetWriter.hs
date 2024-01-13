module Gourmet.GourmetWriter (writeGourmet) where

-- writeGourmet :: Int
-- writeGourmet = 1

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Writer
import Syntax

type GourmetWriter = Writer String

-- Helper funcs

addIndents :: Int -> String
addIndents n = replicate n '\t'

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
   writeExp expr1 >> tell "." >> writeExp expr2

writeStruct :: Struct -> GourmetWriter ()
writeStruct (Struct name args) = do
    tell $ "struct " ++ name ++ "("
    case length args of
        0 -> tell ")"
        1 -> do
            writeExp $ head args
            tell ")"
        _ -> do
            mapM_ (\a -> (writeExp a) >> tell ", " ) (init args)
            (writeExp (last args)) >> tell ")"

-- Values

writeValue :: Value -> GourmetWriter ()
writeValue (Nil) = tell "nil"
writeValue (Boolean bool) =
    tell $ if bool == True then "true" else "false" -- kanskje $ ikke nødvendig
writeValue (Number n) = tell $ show n
writeValue (Text str) = tell str
writeValue (List exprs) = do
    tell "["
    case length exprs of
        0 -> tell "]"
        1 -> (writeExp $ head exprs) >> tell "]"
        _ -> do
            mapM_ (\expr -> (writeExp expr) >> tell ", ") (init exprs)
            (writeExp $ last exprs) >> tell "]"
writeValue (HashSet exprs) = do
    tell "set{"
    let set = Set.toList exprs
    case length exprs of
        0 -> tell "}"
        1 -> (writeExp $ head set) >> tell "}"
        _ -> do
            mapM_ (\expr -> (writeExp expr) >> tell ", ") (init set)
            (writeExp $ last set) >> tell "}"
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
writePair (x, y) = writeExp x >> tell ": " >> writeExp y

-- Expressions

writeExp :: Expression -> GourmetWriter ()
writeExp (Constant v) = writeValue v
writeExp (VariableExp var) = tell var
writeExp (BinaryExp op exp1 exp2) = do
    writeExp exp1
    writeOp op
    writeExp exp2
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
writeExp (LocalStructField _) = undefined -- This can't happen since they aren't parsed

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
writeAssignmentTarget (ListIndexTarget var indexes) = do
    tell var
    mapM_ (\x -> tell "[" >> writeExp x >> tell "]") indexes >> tell " "
writeAssignmentTarget (StructFieldTarget struct) =
    writeStructField struct

writeAssignmentValue :: AssignmentValue -> GourmetWriter ()
writeAssignmentValue (ExpressionValue expr) = writeExp expr
writeAssignmentValue (StructValue struct) = writeStruct struct

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
getArguments = map (\(Argument name t) -> name ++ " " ++ t)

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
    mapM_ (\s -> (writeStructDecl s) >> tell "\n\n") structs
    mapM_ (\f -> (writeFunc f) >> tell "\n\n") funcs
    writeFunctionCall funcCall