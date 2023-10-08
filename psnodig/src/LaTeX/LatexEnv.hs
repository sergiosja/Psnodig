module LaTeX.LatexEnv (extractEnv) where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (isPrefixOf)
import Syntax

type Collector = State (Set String, Set String, Set String)

collectNames :: Program -> Collector ()
collectNames (Program structs funcs _) = do
    mapM_ collectStructDeclarations structs
    mapM_ collectFuncDeclarations funcs

collectStructDeclarations :: Struct -> Collector ()
collectStructDeclarations (Struct name _) = do
    modify(\(structs, funcs, arrays) -> (Set.insert name structs, funcs, arrays))

collectFuncDeclarations :: Function -> Collector ()
collectFuncDeclarations (Function name args stmts) = do
    modify (\(structs, funcs, arrays) -> (structs, Set.insert name funcs, arrays))
    mapM_ collectArgs args
    mapM_ collectStmts stmts

collectArgs :: Argument -> Collector ()
collectArgs arg = case arg of
    (ArrayArg name _ ) ->
        modify (\(structs, funcs, arrays) -> (structs, funcs, Set.insert name arrays))
    _ -> return ()

collectExpr :: Expression -> Collector ()
collectExpr arg =
    case arg of
        (BinaryExp _ exp1 exp2) -> do
            collectExpr exp1
            collectExpr exp2
        (ArrayIndex _ expr) ->
            collectExpr expr
        (CallExp (FunctionCall name exps)) -> do
            modify (\(structs, funcs, arrays) -> (structs, Set.insert name funcs, arrays))
            mapM_ collectExpr exps
        (Not expr) ->
            collectExpr expr
        _ -> return ()

collectStmts :: Statement -> Collector ()
collectStmts stmt =
    case stmt of
        (Assignment (VariableTarget name) (ExpressionValue (ArrayExp (FullArray entries)))) -> do
            modify (\(structs, funcs, arrays) -> (structs, funcs, Set.insert name arrays))
            mapM_ collectExpr entries
        (Assignment (VariableTarget name) (ExpressionValue (ArrayExp (EmptyArray _)))) -> do
            modify (\(structs, funcs, arrays) -> (structs, funcs, Set.insert name arrays))
        (Assignment (VariableTarget name) (HashMapValue _)) -> do
            modify (\(structs, funcs, arrays) -> (structs, funcs, Set.insert name arrays))
        (Assignment (VariableTarget name) (HashSetValue _)) -> do
            modify (\(structs, funcs, arrays) -> (structs, funcs, Set.insert name arrays))
        (Assignment (VariableTarget _) (ExpressionValue expr)) ->
            collectExpr expr
        (Loop expr stmts) -> do
            collectExpr expr
            mapM_ collectStmts stmts
        (If expr stmts maybeElse) -> do
            collectExpr expr
            mapM_ collectStmts stmts
            collectElse maybeElse
        (ForEach _ expr stmts) -> do
            collectExpr expr
            mapM_ collectStmts stmts
        (For _ expr1 expr2 stmts) -> do
            collectExpr expr1
            collectExpr expr2
            mapM_ collectStmts stmts
        (CallStmt (FunctionCall name _)) ->
            modify (\(structs, funcs, arrays) -> (structs, Set.insert name funcs, arrays))
        (Return expr) ->
            collectExpr expr
        (HashStmt stmnt) ->
            collectStmts stmnt
        (AnnotationStmt _ stmts) ->
            mapM_ collectStmts stmts
        _ -> return ()

collectElse :: Maybe Else -> Collector ()
collectElse elsePart =
    case elsePart of
        (Just (ElseIf expr stmts maybeElse)) -> do
            collectExpr expr
            mapM_ collectStmts stmts
            collectElse maybeElse
        (Just (Else stmts)) ->
            mapM_ collectStmts stmts
        Nothing -> return ()

extractEnv :: Program -> ([String], [String], [String])
extractEnv program =
    let (structs, funcs, arrays) = execState (collectNames program) (Set.empty, Set.empty, Set.empty)
    in (Set.toList structs, Set.toList funcs, Set.toList arrays)