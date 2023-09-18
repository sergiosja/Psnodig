module LaTeX.LatexWriter (writeLatex) where

import Control.Monad.Writer
import Syntax

type LatexWriter = Writer String

-- Helper funcs

addIndents :: Int -> String
addIndents n = replicate n '\t'

-- Expressions

writeExp :: Expression -> LatexWriter ()
writeExp (Constant n) = tell $ show n
writeExp (VariableExp var) = tell $ "\\var{" ++ var ++ "}"
writeExp (BinaryExp op exp1 exp2) = do
    writeExp exp1 >> transpileOp op >> writeExp exp2
writeExp (Boolean val) = case val of
    True -> tell "\\KwTrue"
    False -> tell "\\KwFalse"
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
writeExp (ArrayExp name index) = do -- fix this later when reader monad stores arrays
    tell $ "\\var{" ++ name ++ "}["
    writeExp index
    tell "]"

writeFunctionCall :: FunctionCall -> LatexWriter ()
writeFunctionCall (FunctionCall funcname args) = do
    tell $ funcname ++ "(" -- her vil funcname mappe til FuncName eller noe, så må vi hente det. prefix \\
    case length args of
        0 -> tell ")"
        1 -> (writeExp $ head args) >> tell ")"
        _ -> do
            mapM_ (\arg -> (writeExp arg) >> tell ", ") (init args)
            (writeExp $ last args) >> tell ")"

-- Statements

writeStmt :: Statement -> Int -> LatexWriter ()
writeStmt (Assignment var expr) _ = do
    tell $ "$\\var{" ++ var ++ "} "
    tell "\\gets "
    writeExp expr
    tell "$ \\;"
writeStmt (Loop expr stmts) indent = do
    tell "\\While{$"
    writeExp expr
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
writeStmt (Print expr) _ = do
    tell "\\KwPrint $"
    writeExp expr
    tell "$ \\;"

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

-- \proc{$\BinarySearch(\A, x)$}{}
writeFunc :: Function -> LatexWriter ()
writeFunc (Function funcname args stmts) = do
    tell $ "\\proc{$" ++ funcname ++ "(" -- when we save functions, add \\ prefix to funcname
    case length args of
        0 -> tell ")$}{\n"
        1 -> tell $ head args ++ ")$}{\n"
        _ -> do
            mapM_ (\a -> (tell $ a ++ ", ")) (init args)
            tell $ last args ++ ")$}{\n"
    mapM_ (\stmt -> tell "\t" >> writeStmt stmt 1 >> tell "\n") stmts
    tell "}"

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


-- Static stuff

constantConfig :: LatexWriter ()
constantConfig =
    tell "\\documentclass{standalone}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath,commath} \n\\usepackage[linesnumbered, ruled]{algorithm2e}\n\\SetKwProg{proc}{Procedure}{}{}\n\\SetKw{KwFalse}{false}\n\\SetKw{KwTrue}{true}\n\\SetKw{KwPrint}{print}\n\\newcommand{\\var}{\\texttt}\n\\DontPrintSemicolon\n\\begin{document}\n\n"

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
