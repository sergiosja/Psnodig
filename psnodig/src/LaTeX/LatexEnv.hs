module LaTeX.LatexEnv (extractEnv) where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax

type Collector = State (Set String, Set String)

collectNames :: Program -> Collector ()
collectNames (Program funcs _) =
    mapM_ collectFuncDeclarations funcs

collectFuncDeclarations :: Function -> Collector ()
collectFuncDeclarations (Function name args stmts) = do
    modify (\(funcs, arrays) -> (Set.insert name funcs, arrays))
    mapM_ collectArgs args
    mapM_ collectStmts stmts

collectArgs :: FunctionArg -> Collector ()
collectArgs arg = case arg of
    (ArrayArg name _ ) ->
        modify (\(funcs, arrays) -> (funcs, Set.insert name arrays))
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
            modify (\(funcs, arrays) -> (Set.insert name funcs, arrays))
            mapM_ collectExpr exps
        (Not expr) ->
            collectExpr expr
        _ -> return ()

collectStmts :: Statement -> Collector ()
collectStmts stmt =
    case stmt of
        (Assignment (VariableTarget name) (ArrayExp (Array entries))) -> do
            modify (\(funcs, arrays) -> (funcs, Set.insert name arrays))
            mapM_ collectExpr entries
        (Assignment (VariableTarget _) expr) ->
            collectExpr expr
        (Loop expr stmts) -> do
            collectExpr expr
            mapM_ collectStmts stmts
        (If expr stmts maybeElse) -> do
            collectExpr expr
            mapM_ collectStmts stmts
            collectElse maybeElse
        (ForEach _ _ stmts) ->
            mapM_ collectStmts stmts
        (For _ expr1 expr2 stmts) -> do
            collectExpr expr1
            collectExpr expr2
            mapM_ collectStmts stmts
        (CallStmt (FunctionCall name _)) ->
            modify (\(funcs, arrays) -> (Set.insert name funcs, arrays))
        (Return expr) ->
            collectExpr expr
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

extractEnv :: Program -> ([String], [String])
extractEnv program =
    let (funcs, arrays) = execState (collectNames program) (Set.empty, Set.empty)
    in (Set.toList funcs, Set.toList arrays)