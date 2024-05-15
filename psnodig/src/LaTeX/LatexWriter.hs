module LaTeX.LatexWriter
    ( writeLatex
    , writeValue
    , writeProgramDescription
    , writeExpr
    , writeStmt
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (toUpper)
import Control.Monad.Reader
import Control.Monad.Writer
import Syntax

type Environment = ([String], [String])
type LatexWriter = ReaderT Environment (Writer String)

-- Helper funcs

addIndents :: Int -> String
addIndents n = replicate n '\t'

writeFuncArgs :: [Expression] -> LatexWriter ()
writeFuncArgs [] = tell ")"
writeFuncArgs [x] = writeExpr x False >> tell ")"
writeFuncArgs (x:xs) = writeExpr x False >> tell ", " >> writeFuncArgs xs

-- Structs

writeStructField :: StructField -> LatexWriter ()
writeStructField (StructField expr1 expr2) =
    writeExpr expr1 True >> tell "_{" >> writeExpr expr2 True >> tell "}"

writeStruct :: Struct -> LatexWriter ()
writeStruct (Struct name args) = do
    tell $ "\\" ++ name ++ "("
    case length args of
        0 -> tell ")"
        1 -> do
            writeExpr (head args) False
            tell ")"
        _ -> do
            mapM_ (\a -> (writeExpr a False) >> tell ", ") (init args)
            (writeExpr (last args) False) >> tell ")"

-- Values

writeValue :: Value -> LatexWriter ()
writeValue (Nil) = tell "\\KwNil"
writeValue (Boolean bool) =
    tell $ if bool == True then "\\KwTrue" else "\\KwFalse"
writeValue (Number n) = tell $ show n
writeValue (Decimal d) = tell $ show d
writeValue (Text str) = tell $ show str
writeValue (List exprs) = do
    tell "["
    case length exprs of
        0 -> tell "]"
        1 -> (writeExpr (head exprs) False) >> tell "]"
        _ -> do
            mapM_ (\expr -> (writeExpr expr False) >> tell ", ") (init exprs)
            (writeExpr (last exprs) False) >> tell "]"
writeValue (HashSet exprs) = do
    tell "\\{"
    let set = Set.toList exprs
    case length exprs of
        0 -> tell "\\}"
        1 -> (writeExpr (head set) False) >> tell "\\}"
        _ -> do
            mapM_ (\expr -> (writeExpr expr False) >> tell ", ") (init set)
            (writeExpr (last set) False) >> tell "\\}"
writeValue (HashMap hmap) = do
    tell "\\{"
    let pairs = Map.toList hmap
    case length pairs of
        0 -> tell "\\}"
        1 -> (writePair $ head pairs) >> tell "\\}"
        _ -> do
            mapM_ (\p -> (writePair p) >> tell ", ") (init pairs)
            (writePair $ last pairs) >> tell "\\}"
writeValue (StructVal _) = undefined -- these aren't parsed

writePair :: (Expression, Expression) -> LatexWriter ()
writePair (x, y) = writeExpr x False >> tell ": " >> writeExpr y False

-- Expressions

writeExpr :: Expression -> Bool -> LatexWriter ()
writeExpr (Constant v) _ = writeValue v
writeExpr (VariableExp var) _ = tell var
writeExpr (BinaryExp op exp1 exp2) fromMaths = do
    case op of
        Division -> do
            tell $ if not fromMaths then "$" else ""
            tell "\\frac{"
            writeExpr exp1 True >> tell "}{"
            writeExpr exp2 True >> tell "}"
            tell $ if not fromMaths then "$" else ""
        _ -> do
            writeExpr exp1 fromMaths
            if not fromMaths then tell " $" >> transpileOp op >> tell "$ "
            else tell " " >> transpileOp op >> tell " "
            writeExpr exp2 fromMaths
writeExpr (CallExp functioncall) _ = do
    writeFunctionCall functioncall
writeExpr (ListIndex name indexes) _ = do
    tell name
    mapM_ (\x -> tell "[" >> writeExpr x False >> tell "]") indexes
writeExpr (Not expr) fromMaths = do
    case expr of
        (CallExp (FunctionCall "in" args)) -> do
            writeExpr (head args) False
            tell $ if fromMaths then " \\notin " else " $\\notin$ "
            writeExpr (last args) False
        _ -> do
            tell "\\KwNot "
            writeExpr expr False
writeExpr (StructExpr struct) _ =
    writeStruct struct
writeExpr (StructFieldExp structField) fromMaths =
    if fromMaths
    then writeStructField structField
    else tell "$" >> writeStructField structField >> tell "$"

-- Function related

writeFunctionCall :: FunctionCall -> LatexWriter ()
writeFunctionCall (FunctionCall funcname args) = do
    case funcname of
        "length" -> do
            tell "\\abs{"
            writeExpr (head args) True
            tell "}"
        "ceil" -> do
            tell "$\\lceil$"
            writeExpr (head args) False
            tell "$\\rceil$"
        "floor" -> do
            tell "$\\lfloor$"
            writeExpr (head args) False
            tell "$\\rfloor$"
        "in" -> do
            writeExpr (head args) False
            tell $ " \\in "
            writeExpr (last args) False
            tell "?"
        "append" -> do
            tell "append "
            writeExpr (head args) False
            tell " to "
            writeExpr (last args) False
        "add" -> do
            tell "add "
            if length args == 2
            then do
                tell "("
                writeExpr (head args) False >> tell ", "
                writeExpr (args !! 1) False >> tell ")"
            else
                writeExpr (head args) False
            tell " to "
            writeExpr (last args) False
        _ -> do
            tell $ "\\" ++ funcname ++ "("
            writeFuncArgs args

getArgumentNames ::  [Argument] -> [String]
getArgumentNames args = map getArgName args
    where getArgName (Argument name _) = name

writeFunc :: FunctionDecl -> LatexWriter ()
writeFunc (FunctionDecl funcname args stmts) = do
    tell $ "\\proc{$\\" ++ funcname ++ "("
    case length args of
        0 -> tell ")$}{\n"
        1 -> tell $ head (getArgumentNames args) ++ ")$}{\n"
        _ -> do
            mapM_ (\a -> (tell $ a ++ ", ")) (init $ getArgumentNames args)
            tell $ last (getArgumentNames args) ++ ")$}{\n"
    mapM_ (\stmt -> tell "\t" >> writeStmt stmt 1 >> tell "\n") stmts
    tell "}"

-- Statements

writeStmt :: Statement -> Int -> LatexWriter ()
writeStmt (Assignment target value) _ = do
    writeAssignmentTarget target
    tell " $\\gets$ "
    writeAssignmentValue value
    tell " \\;"
writeStmt (Loop expr stmts) indent = do
    tell "\\While{$"
    writeExpr expr True
    tell "$}{\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (ForEach item expr stmts) indent = do
    tell $ "\\For{$" ++ item ++ " \\in "
    writeExpr expr True
    tell "$}{\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (For item from to stmts) indent = do
    tell $ "\\For{$" ++ item ++ " \\gets "
    writeExpr from True
    tell $ "$ \\KwTo $"
    writeExpr to True
    tell "$}{\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (If expr stmts maybeElse) indent = do
    tell "\\uIf{$"
    writeExpr expr True
    tell "$}{\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
    case maybeElse of
        Just elsePart -> writeElse elsePart indent
        Nothing -> return ()
writeStmt (Return expr) _ = do
    tell "\\Return "
    writeExpr expr False
    tell " \\;"
writeStmt (CallStmt functioncall) _ = do
    writeFunctionCall functioncall
    tell " \\;"
writeStmt (HashStmt _) _ = return ()
writeStmt (AnnotationStmt description _) _ =
    tell $ "\\text{" ++ description ++ "} \\;"
writeStmt Break _ =
    tell "\\KwBreak \\;"
writeStmt Continue _ =
    tell "\\KwContinue \\;"

writeAssignmentTarget :: AssignmentTarget -> LatexWriter ()
writeAssignmentTarget (VariableTarget var) = tell $ "\\texttt{" ++ var ++ "}"
writeAssignmentTarget (ListIndexTarget var indexes) = do
    tell var
    mapM_ (\x -> tell "[" >> writeExpr x False >> tell "]") indexes
writeAssignmentTarget (StructFieldTarget struct) = do
    tell "$"
    writeStructField struct
    tell "$"

writeAssignmentValue :: AssignmentValue -> LatexWriter ()
writeAssignmentValue (ExpressionValue expr) = writeExpr expr False
writeAssignmentValue (StructValue struct) = writeStruct struct

writeElse :: Else -> Int -> LatexWriter ()
writeElse (ElseIf expr stmts maybeElse) indent = do
    tell $ "\n" ++ (addIndents indent)
    tell "\\uElseIf{$" >> writeExpr expr True >> tell "$}{\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
    case maybeElse of
        Just elsePart -> writeElse elsePart indent
        Nothing -> return ()
writeElse (Else stmts) indent = do
    tell $ "\n" ++ (addIndents indent)
    tell "\\uElse{\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"


-- Operators

transpileOp :: Operator -> LatexWriter ()
transpileOp op = tell $ case op of
    Plus             -> "+"
    Minus            -> "-"
    Times            -> "\\cdot"
    Division         -> "/"
    LessThan         -> "<"
    LessThanEqual    -> "\\leq"
    GreaterThan      -> ">"
    GreaterThanEqual -> "\\geq"
    Equal            -> "="
    NotEqual         -> "\\neq"
    And              -> "\\land"
    Or               -> "\\lor"
    Modulo           -> "\\%"


-- Static stuff

writeFunctionDecls :: LatexWriter ()
writeFunctionDecls = do
    funcs <- asks fst
    mapM_ (\f -> tell $ "\\SetKwFunction{" ++ f ++ "}{" ++ f ++ "}\n") funcs

writeKeywords :: LatexWriter ()
writeKeywords = do
    keywords <- asks snd
    mapM_ (\k -> tell $ "\\SetKw{Kw" ++ fstToUpper k ++ "}{" ++ k ++ "}\n") keywords

fstToUpper :: String -> String
fstToUpper "" = ""
fstToUpper str =
    toUpper (head str) : tail str

constantConfig :: LatexWriter ()
constantConfig = do
    tell "\\documentclass{standalone}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath,commath} \n\\usepackage[linesnumbered, ruled]{algorithm2e}\n\\SetKwProg{proc}{Procedure}{}{}\n"
    writeFunctionDecls >> writeKeywords
    tell "\\DontPrintSemicolon\n\\renewcommand{\\thealgocf}{}\n\\begin{document}\n\n\\begin{algorithm}[H]\n"

writeProgramDescription :: Maybe ProgramDescription -> LatexWriter ()
writeProgramDescription Nothing = return ()
writeProgramDescription (Just (ProgramDescription input output)) = do
    tell $ "\\KwIn{" ++ input ++ "}\n"
    tell $ "\\KwOut{" ++ output ++ "}\n"

funcEnd :: FunctionDecl -> LatexWriter ()
funcEnd (FunctionDecl name _ _) =
    tell $ "\n\\caption{" ++ name ++ "}\n\\end{algorithm}\n\n\\end{document}"

writeLatex :: Program -> LatexWriter ()
writeLatex (Program programDescription _ funcs _) =
    if null funcs then return ()
    else do
        constantConfig
        writeProgramDescription programDescription
        writeFunc (head funcs)
        funcEnd (head funcs)