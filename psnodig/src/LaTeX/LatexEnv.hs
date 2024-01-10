module LaTeX.LatexEnv (extractEnv) where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Syntax

type Collector = State (Set String, Set String, Set String)

collectNames :: Program -> Collector ()
collectNames (Program structs funcs _) = do
    mapM_ collectStructDeclarations structs
    mapM_ collectFuncDeclarations funcs

collectStructDeclarations :: StructDecl -> Collector ()
collectStructDeclarations (StructDecl name _) = do
    modify(\(structs, funcs, lists) -> (Set.insert name structs, funcs, lists))

collectFuncDeclarations :: Function -> Collector ()
collectFuncDeclarations (Function name args stmts) = do
    modify (\(structs, funcs, lists) -> (structs, Set.insert name funcs, lists))
    mapM_ collectArgs args
    mapM_ collectStmts stmts

collectArgs :: Argument -> Collector ()
collectArgs (Argument n t) = case t of
    "bool" -> return ()
    "int" -> return ()
    "string" -> return ()
    _ ->
        modify (\(structs, funcs, lists) -> (structs, funcs, Set.insert n lists))

collectStmts :: Statement -> Collector ()
collectStmts stmt =
    case stmt of
        (Assignment (VariableTarget name) (ExpressionValue (Constant value))) -> do
            modify (\(structs, funcs, lists) -> (structs, funcs, Set.insert name lists))
            case value of
                (List exprs) -> mapM_ collectExpr exprs
                (HashSet set) ->
                    let exprs = Set.toList set
                    in mapM_ collectExpr exprs
                (HashMap hmap) ->
                    let pairs = Map.toList hmap
                    in mapM_ collectPair pairs
                _ -> return ()
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
            modify (\(structs, funcs, lists) -> (structs, Set.insert name funcs, lists))
        (Return expr) ->
            collectExpr expr
        (HashStmt stmnt) ->
            collectStmts stmnt
        (AnnotationStmt _ stmts) ->
            mapM_ collectStmts stmts
        _ -> return ()

collectExpr :: Expression -> Collector ()
collectExpr arg =
    case arg of
        (BinaryExp _ exp1 exp2) -> do
            collectExpr exp1
            collectExpr exp2
        (ListIndex list exprs) -> do
            modify (\(structs, funcs, lists) -> (structs, funcs, Set.insert list lists))
            mapM_ collectExpr exprs
        (CallExp (FunctionCall name exps)) -> do -- if f = contains(A, x), add A to lists
            modify (\(structs, funcs, lists) -> (structs, Set.insert name funcs, lists))
            mapM_ collectExpr exps
        (Not expr) ->
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

collectPair :: (Expression, Expression) -> Collector ()
collectPair (x, y) = do
    collectExpr x
    collectExpr y

extractEnv :: Program -> ([String], [String], [String])
extractEnv program =
    let (structs, funcs, lists) = execState (collectNames program) (Set.empty, Set.empty, Set.empty)
    in (Set.toList structs, Set.toList funcs, Set.toList lists)