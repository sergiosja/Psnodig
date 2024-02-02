module LaTeX.LatexWriter (writeLatex) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Monad.Writer
import Syntax

type Environment = ([String], [String], [String])
type LatexWriter = ReaderT Environment (Writer String)

-- Helper funcs

addIndents :: Int -> String
addIndents n = replicate n '\t'

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, y, _) = y

thrd' :: (a, b, c) -> c
thrd' (_, _, z) = z

-- Structs

writeStructField :: StructField -> LatexWriter ()
writeStructField (StructField expr1 expr2) =
    writeExp expr1 >> tell "_{" >> writeExp expr2 >> tell "} "

writeStruct :: Struct -> LatexWriter ()
writeStruct (Struct name args) = do
    tell $ "\\" ++ name ++ "("
    case length args of
        0 -> tell ")"
        1 -> do
            writeExp $ head args
            tell ")"
        _ -> do
            mapM_ (\a -> (writeExp a) >> tell ", ") (init args)
            (writeExp $ last args) >> tell ")"

-- Values

writeValue :: Value -> LatexWriter ()
writeValue (Nil) = tell "\\KwNil"
writeValue (Boolean bool) =
    tell $ if bool == True then "\\top" else "\\bot" -- kanskje $ ikke nødvendig -- alt \\KwTrue \\KwFalse
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
    tell "\\{"
    let set = Set.toList exprs
    case length exprs of
        0 -> tell "\\}"
        1 -> (writeExp $ head set) >> tell "\\}"
        _ -> do
            mapM_ (\expr -> (writeExp expr) >> tell ", ") (init set)
            (writeExp $ last set) >> tell "\\}"
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
writePair (x, y) = writeExp x >> tell ": " >> writeExp y

-- Expressions

writeExp :: Expression -> LatexWriter ()
writeExp (Constant v) = writeValue v
writeExp (VariableExp var) = tell var
writeExp (BinaryExp op exp1 exp2) = do
    case op of
        Division -> do
            tell "\\frac{"
            writeExp exp1 >> tell "}{"
            writeExp exp2 >> tell "}"
        _ -> writeExp exp1 >> transpileOp op >> writeExp exp2
writeExp (CallExp functioncall) = do
    writeFunctionCall functioncall
writeExp (ListIndex name indexes) = do
    tell $ "\\" ++ name
    mapM_ (\x -> tell "[" >> writeExp x >> tell "]") indexes
writeExp (Not expr) = do
    case expr of
        (CallExp (FunctionCall "contains" args)) -> do
            lists <- asks thrd' -- check fst' & snd' too?
            writeExp $ last args
            tell $ " \\notin "
            writeExp $ collectionVariableNotation (head args) lists
        _ -> do
            tell "\\KwNot \\: "
            writeExp expr
writeExp (StructExpr struct) =
    writeStruct struct
writeExp (StructFieldExp structField) =
    writeStructField structField

-- Function related

collectionVariableNotation :: Expression -> [String] -> Expression
collectionVariableNotation expr collection =
    case expr of
        (VariableExp x) -> if elem x collection then (VariableExp ("\\" ++ x)) else (VariableExp x)
        _ -> expr

writeFunctionCall :: FunctionCall -> LatexWriter ()
writeFunctionCall (FunctionCall funcname args) = do
    lists <- asks thrd' -- check fst' & snd' too?
    case funcname of
        "length" -> do
            tell "\\abs{"
            writeExp $ collectionVariableNotation (head args) lists
            tell "}"
        "ceil" -> do
            tell "\\lceil "
            writeExp $ collectionVariableNotation (head args) lists
            tell "\\rceil"
        "floor" -> do
            tell "\\lfloor "
            writeExp $ collectionVariableNotation (head args) lists
            tell "\\rfloor"
        "contains" -> do
            writeExp $ last args
            tell $ " \\in "
            writeExp $ collectionVariableNotation (head args) lists
        "append" -> do
            tell "append "
            writeExp $ args !! 1
            tell " to "
            writeExp $ args !! 0
        "add" -> do
            tell "add "
            writeExp $ args !! 1
            tell " to "
            writeExp $ args !! 0
        _ -> do
            tell $ "\\" ++ funcname ++ "("
            case length args of
                0 -> tell ")"
                1 -> (writeExp $ collectionVariableNotation (head args) lists) >> tell ")"
                _ -> do
                    mapM_ (\arg -> (writeExp $ collectionVariableNotation arg lists) >> tell ", ") (init args)
                    (writeExp $ collectionVariableNotation (last args) lists) >> tell ")"

getArgumentNames ::  [Argument] -> [String]
getArgumentNames args = map getArgName args
    where
        getArgName (Argument name t) = case t of
            "bool" -> name
            "int" -> name
            "string" -> name
            _ -> "\\" ++ name

writeFunc :: Function -> LatexWriter ()
writeFunc (Function funcname args stmts) = do
    tell $ "\\proc{$\\" ++ funcname ++ "("
    case length args of
        0 -> tell ")$}{\n"
        1 -> do
            tell $ head (getArgumentNames args) ++ ")$}{\n"
        _ -> do
            mapM_ (\a -> (tell $ a ++ ", ")) (init $ getArgumentNames args)
            tell $ last (getArgumentNames args) ++ ")$}{\n"
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
writeStmt (ForEach item expr stmts) indent = do
    tell $ "\\For{$" ++ item ++ " \\in "
    writeExp expr
    tell "$}{\n"
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
writeAssignmentTarget (ListIndexTarget var indexes) = do
    tell $ "$\\" ++ var
    mapM_ (\x -> tell "[" >> writeExp x >> tell "]") indexes >> tell " "
writeAssignmentTarget (StructFieldTarget struct) = do
    tell "$"
    writeStructField struct

writeAssignmentValue :: AssignmentValue -> LatexWriter ()
writeAssignmentValue (ExpressionValue expr) = writeExp expr
writeAssignmentValue (StructValue struct) = writeStruct struct

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
    Times            -> " \\cdot "
    Division         -> " / "
    LessThan         -> " < "
    LessThanEqual    -> " \\leq "
    GreaterThan      -> " > "
    GreaterThanEqual -> " \\geq "
    Equal            -> " = "
    NotEqual         -> " \\neq "
    And              -> " \\land "
    Or               -> " \\lor "
    Modulo           -> " % "


-- Static stuff

writeStaticFunctions :: LatexWriter ()
writeStaticFunctions = do
    funcs <- asks snd'
    mapM_ (\f -> tell $ "\\SetKwFunction{" ++ f ++ "}{" ++ f ++ "}\n") funcs
    structs <- asks fst'
    mapM_ (\s -> tell $ "\\SetKwFunction{" ++ s ++ "}{" ++ s ++ "}\n") structs

writeStaticLists :: LatexWriter ()
writeStaticLists = do
    lists <- asks thrd'
    mapM_ (\a -> tell $ "\\SetKwArray{" ++ a ++ "}{" ++ a ++ "}\n") lists

constantConfig :: LatexWriter ()
constantConfig = do
    tell "\\documentclass{standalone}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath,commath} \n\\usepackage[linesnumbered, ruled]{algorithm2e}\n\\SetKwProg{proc}{Procedure}{}{}\n"
    writeStaticFunctions
    writeStaticLists
    tell "\\SetKw{KwNil}{Nil}\n\\SetKw{KwContinue}{continue}\n\\SetKw{KwBreak}{break}\n\\SetKw{KwFalse}{false}\n\\SetKw{KwTrue}{true}\n\\SetKw{KwNot}{not}\n\\SetKw{KwTo}{to}\n\\newcommand{\\var}{\\texttt}\n\\DontPrintSemicolon\n\\begin{document}\n\n"

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