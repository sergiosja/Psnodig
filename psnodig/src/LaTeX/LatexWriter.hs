module LaTeX.LatexWriter (writeLatex) where

import Control.Monad.Reader
import Control.Monad.Writer
import Syntax

type Environment = ([String], [String])
type LatexWriter = ReaderT Environment (Writer String)

-- Helper funcs

addIndents :: Int -> String
addIndents n = replicate n '\t'

-- Expressions

writeExp :: Expression -> LatexWriter ()
writeExp (Constant n) = tell $ show n
writeExp (VariableExp var) = tell var
writeExp (BinaryExp op exp1 exp2) = do
    writeExp exp1 >> transpileOp op >> writeExp exp2
writeExp (Boolean val) = case val of
    True -> tell "\\KwTrue"
    False -> tell "\\KwFalse"
writeExp (CallExp functioncall) = do
    writeFunctionCall functioncall
writeExp (ArrayExp array) =
    writeArray array
writeExp (ArrayIndex name index) = do
    tell $ "\\" ++ name ++ "{"
    writeExp index
    tell "}"
writeExp (Not expr) = do
    tell "\\KwNot "
    writeExp expr

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

getFuncArgs :: [FunctionArg] -> [String]
getFuncArgs = map getArgName
    where
        getArgName (ArrayArg name _) = "\\" ++ name
        getArgName (IntArg name _) = name

writeFunc :: Function -> LatexWriter ()
writeFunc (Function funcname args stmts) = do
    tell $ "\\proc{$\\" ++ funcname ++ "("
    case length args of
        0 -> tell ")$}{\n"
        1 -> tell $ head (getFuncArgs args) ++ ")$}{\n"
        _ -> do
            mapM_ (\a -> (tell $ a ++ ", ")) (init $ getFuncArgs args)
            tell $ last (getFuncArgs args) ++ ")$}{\n"
    mapM_ (\stmt -> tell "\t" >> writeStmt stmt 1 >> tell "\n") stmts
    tell "}"

-- Statements

writeStmt :: Statement -> Int -> LatexWriter ()
writeStmt (Assignment target expr) _ = do
    writeAssignmentTarget target
    tell "\\gets "
    writeExp expr
    tell "$ \\;"
writeStmt (Loop expr stmts) indent = do
    tell "\\While{$"
    writeExp expr
    tell "$}{\n"
    mapM_ (\stmt -> (tell $ addIndents $ indent+1) >> writeStmt stmt (indent+1) >> tell "\n") stmts
    tell $ (addIndents indent) ++ "}"
writeStmt (ForEach item array stmts) indent = do
    tell $ "\\For{$" ++ item ++ "$ \\forin $\\" ++ array ++ "$}{\n"
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
writeStmt Pass _ = do
    tell "$\\var{pass}$ \\;"
writeStmt (Return expr) _ = do
    tell "\\Return $"
    writeExp expr
    tell "$"
writeStmt (CallStmt functioncall) _ = do
    writeFunctionCall functioncall
    tell " \\;"

writeAssignmentTarget :: AssignmentTarget -> LatexWriter ()
writeAssignmentTarget (VariableTarget var) = tell $ "$\\var{" ++ var ++ "} "
writeAssignmentTarget (ArrayIndexTarget var expr) = do
    tell $ "$\\" ++ var ++ "{"
    writeExp expr
    tell "}"

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
    funcs <- asks fst
    mapM_ (\f -> tell $ "\\SetKwFunction{" ++ f ++ "}{" ++ f ++ "}\n") funcs

writeStaticArrays :: LatexWriter ()
writeStaticArrays = do
    arrays <- asks snd
    mapM_ (\a -> tell $ "\\SetKwArray{" ++ a ++ "}{" ++ a ++ "}\n") arrays

constantConfig :: LatexWriter ()
constantConfig = do
    tell "\\documentclass{standalone}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath,commath} \n\\usepackage[linesnumbered, ruled]{algorithm2e}\n\\SetKwProg{proc}{Procedure}{}{}\n"
    writeStaticFunctions
    writeStaticArrays
    tell "\\SetKw{KwFalse}{false}\n\\SetKw{KwTrue}{true}\n\\SetKw{KwNot}{not}\n\\SetKw{KwTo}{to}\n\\SetKw{forin}{in}\n\\newcommand{\\var}{\\texttt}\n\\DontPrintSemicolon\n\\begin{document}\n\n"

funcStart :: LatexWriter ()
funcStart =
    tell "\\begin{algorithm}[H]\n\\KwIn{Input}\n\\KwOut{Output}\n"

funcEnd :: LatexWriter ()
funcEnd =
    tell "\\caption{Hva heter algoritmen?}\n\\end{algorithm}\n\n"

writeLatex :: Program -> LatexWriter ()
writeLatex (Program funcs _) = do
    constantConfig
    mapM_ (\f -> (funcStart >> (writeFunc f) >> funcEnd)) funcs
    tell "\\end{document}"
