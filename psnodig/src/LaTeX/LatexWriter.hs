module LaTeX.LatexWriter (writeLatex) where

import Prelude hiding (fst, snd)
import Control.Monad.Reader
import Control.Monad.Writer
import Syntax

type Environment = ([String], [String], [String])
type LatexWriter = ReaderT Environment (Writer String)

-- Helper funcs

addIndents :: Int -> String
addIndents n = replicate n '\t'

fst :: (a, b, c) -> a
fst (x, _, _) = x

snd :: (a, b, c) -> b
snd (_, y, _) = y

thrd :: (a, b, c) -> c
thrd (_, _, z) = z

-- Structs

writeStructField :: StructField -> LatexWriter ()
writeStructField (StructField struct field) =
    tell $ struct ++ "." ++ field

writeStructAssignment :: StructAssignment -> LatexWriter ()
writeStructAssignment (StructAssignment struct args) = do
    tell $ "\\" ++ struct ++ "("
    case length args of
        0 -> tell ")"
        1 -> do
            writeExp (head args)
            tell ")"
        _ -> do
            mapM_ (\a -> (writeExp a) >> tell ", ") (init args)
            writeExp (last args)
            tell ")"

-- Expressions

writeExp :: Expression -> LatexWriter ()
writeExp (Constant n) = tell $ show n
writeExp (VariableExp var) = tell var
writeExp (BinaryExp op exp1 exp2) = do
    case op of
        Division -> do
            tell "\\frac{"
            writeExp exp1 >> tell "}{"
            writeExp exp2 >> tell "}"
        _ -> writeExp exp1 >> transpileOp op >> writeExp exp2
writeExp (Boolean val) = case val of
    True -> tell "\\KwTrue" -- change to Top?
    False -> tell "\\KwFalse" -- change to Bottom?
writeExp (CallExp functioncall) = do
    writeFunctionCall functioncall
writeExp (ArrayExp array) =
    writeArray array
writeExp (ArrayIndex name index) = do
    tell $ "\\" ++ name ++ "{"
    writeExp index
    tell "}"
writeExp (Not expr) = do
    tell "\\KwNot \\: "
    writeExp expr
    -- if expr == contains, add the math symbol for "not in" (see Lars' graph algos)
writeExp (StructFieldExp struct) =
    writeStructField struct

writeArray :: Array -> LatexWriter ()
writeArray (Array entries) = do
    tell "["
    case length entries of
        0 -> tell "]"
        1 -> (writeExp $ head entries) >> tell "]"
        _ -> do
            mapM_ (\x -> (writeExp x) >> tell ", ") (init entries)
            (writeExp $ last entries) >> tell "]"

-- Function related

arrayVariableNotation :: Expression -> [String] -> Expression
arrayVariableNotation expr arrays =
    case expr of
        (VariableExp x) -> if elem x arrays then (VariableExp ("\\" ++ x)) else (VariableExp x)
        _ -> expr

writeFunctionCall :: FunctionCall -> LatexWriter ()
writeFunctionCall (FunctionCall funcname args) = do
    arrays <- asks snd
    case funcname of
        "length" -> do
            tell "\\abs{"
            writeExp $ arrayVariableNotation (head args) arrays
            tell "}"
        "ceil" -> do
            tell "\\lceil "
            writeExp $ arrayVariableNotation (head args) arrays
            tell "\\rceil"
        "floor" -> do
            tell "\\lfloor "
            writeExp $ arrayVariableNotation (head args) arrays
            tell "\\rfloor"
        "contains" -> do
            writeExp $ last args
            tell $ " \\in "
            writeExp $ arrayVariableNotation (head args) arrays
        _ -> do
            tell $ "\\" ++ funcname ++ "("
            case length args of
                0 -> tell ")"
                1 -> (writeExp $ arrayVariableNotation (head args) arrays) >> tell ")"
                _ -> do
                    mapM_ (\arg -> (writeExp $ arrayVariableNotation arg arrays) >> tell ", ") (init args)
                    (writeExp $ arrayVariableNotation (last args) arrays) >> tell ")"

getArgumentNames :: [String] -> [Argument] -> [String]
getArgumentNames structs arg = map getArgName arg
    where
        getArgName (ArrayArg name _) = "\\" ++ name
        getArgName (SingleArg name _) = do
            case elem name structs of
                True -> "\\" ++ name
                False -> name

writeFunc :: Function -> LatexWriter ()
writeFunc (Function funcname args stmts) = do
    tell $ "\\proc{$\\" ++ funcname ++ "("
    case length args of
        0 -> tell ")$}{\n"
        1 -> do
            structs <- asks fst
            tell $ head (getArgumentNames structs args) ++ ")$}{\n"
        _ -> do
            structs <- asks fst
            mapM_ (\a -> (tell $ a ++ ", ")) (init $ getArgumentNames structs args)
            tell $ last (getArgumentNames structs args) ++ ")$}{\n"
    mapM_ (\stmt -> tell "\t" >> writeStmt stmt 1 >> tell "\n") stmts
    tell "}"

-- Statements

writeStmt :: Statement -> Int -> LatexWriter ()
writeStmt (Assignment target value) _ = do
    writeAssignmentTarget target
    tell "\\gets "
    writeAssignmentValue value
    tell "$ \\;"
writeStmt (Loop expr stmts) indent = do
    tell "\\While{$"
    writeExp expr
    tell "$}{\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (ForEach item array stmts) indent = do
    tell $ "\\For{$" ++ item ++ " \\in \\" ++ array ++ "$}{\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (For item from to stmts) indent = do
    tell $ "\\For{$" ++ item ++ " \\gets "
    writeExp from
    tell $ "$ \\KwTo $"
    writeExp to
    tell "$}{\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (If expr stmts maybeElse) indent = do
    tell "\\uIf{$"
    writeExp expr
    tell "$}{\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
    case maybeElse of
        Just elsePart -> writeElse elsePart indent
        Nothing -> return ()
writeStmt (Return expr) _ = do
    tell "\\Return $"
    writeExp expr
    tell "$ \\;"
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
writeAssignmentTarget (VariableTarget var) = tell $ "$\\var{" ++ var ++ "} "
writeAssignmentTarget (ArrayIndexTarget var expr) = do
    tell $ "$\\" ++ var ++ "{"
    writeExp expr
    tell "}"
writeAssignmentTarget (StructFieldTarget struct) = do
    tell "$"
    writeStructField struct

writeAssignmentValue :: AssignmentValue -> LatexWriter ()
writeAssignmentValue (ExpressionValue expr) = writeExp expr
writeAssignmentValue (StructValue struct) = writeStructAssignment struct

writeElse :: Else -> Int -> LatexWriter ()
writeElse (ElseIf expr stmts maybeElse) indent = do
    tell $ "\n" ++ (addIndents indent)
    tell "\\uElseIf{$" >> writeExp expr >> tell "$}{\n"
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

transpileOp :: Operator -> LatexWriter () -- the outer expression should ALWAYS be wrapped in $
transpileOp op = tell $ case op of
    Plus             -> " + "
    Minus            -> " - "
    Times            -> " * "
    Division         -> " / "
    LessThan         -> " < "
    LessThanEqual    -> " \\leq "
    GreaterThan      -> " > "
    GreaterThanEqual -> " \\geq "
    Equal            -> " = "
    NotEqual         -> " \\neq "
    And              -> " \\land "
    Or               -> " \\lor "


-- Static stuff

writeStaticFunctions :: LatexWriter ()
writeStaticFunctions = do
    funcs <- asks snd
    mapM_ (\f -> tell $ "\\SetKwFunction{" ++ f ++ "}{" ++ f ++ "}\n") funcs
    structs <- asks fst
    mapM_ (\s -> tell $ "\\SetKwFunction{" ++ s ++ "}{" ++ s ++ "}\n") structs

writeStaticArrays :: LatexWriter ()
writeStaticArrays = do
    arrays <- asks thrd
    mapM_ (\a -> tell $ "\\SetKwArray{" ++ a ++ "}{" ++ a ++ "}\n") arrays

constantConfig :: LatexWriter ()
constantConfig = do
    tell "\\documentclass{standalone}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath,commath} \n\\usepackage[linesnumbered, ruled]{algorithm2e}\n\\SetKwProg{proc}{Procedure}{}{}\n"
    writeStaticFunctions
    writeStaticArrays
    tell "\\SetKw{KwContinue}{continue}\n\\SetKw{KwBreak}{break}\n\\SetKw{KwFalse}{false}\n\\SetKw{KwTrue}{true}\n\\SetKw{KwNot}{not}\n\\SetKw{KwTo}{to}\n\\newcommand{\\var}{\\texttt}\n\\DontPrintSemicolon\n\\begin{document}\n\n"

funcStart :: LatexWriter ()
funcStart =
    tell "\\begin{algorithm}[H]\n\\KwIn{Input}\n\\KwOut{Output}\n"

funcEnd :: Function -> LatexWriter ()
funcEnd (Function name _ _) =
    tell $ "\\caption{" ++ name ++ "}\n\\end{algorithm}\n\n"

writeLatex :: Program -> LatexWriter ()
writeLatex (Program _ funcs _) = do
    constantConfig
    mapM_ (\f -> (funcStart >> writeFunc f >> funcEnd f)) funcs
    tell "\\end{document}"