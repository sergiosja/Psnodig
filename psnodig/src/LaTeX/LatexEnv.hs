module LaTeX.LatexEnv (extractEnv) where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Syntax

type Collector = State (Set String, Set String)

-- Top levels

collectNames :: Program -> Collector ()
collectNames (Program _ structs funcs _) = do
    mapM_ collectStructDeclarations structs
    collectFuncDeclarations (head funcs)

collectStructDeclarations :: StructDecl -> Collector ()
collectStructDeclarations (StructDecl name _) = do
    modify(\(funcs, keywords) -> (Set.insert name funcs, keywords))

collectFuncDeclarations :: Function -> Collector ()
collectFuncDeclarations (Function name _ stmts) = do
    modify (\(funcs, keywords) -> (Set.insert name funcs, keywords))
    mapM_ collectStmt stmts

collectFunctionCall :: FunctionCall -> Collector ()
collectFunctionCall (FunctionCall name exprs) = do
    modify (\(funcs, keywords) -> (Set.insert name funcs, keywords))
    mapM_ collectExpr exprs

-- Statements

collectStmt :: Statement -> Collector ()
collectStmt (Assignment target value) = do
    collectAssignmentTarget target
    collectAssignmentValue value
collectStmt (Loop expr stmts) = do
    collectExpr expr
    mapM_ collectStmt stmts
collectStmt (If expr stmts maybeElse) = do
    collectExpr expr
    mapM_ collectStmt stmts
    collectMaybeElse maybeElse
collectStmt (ForEach _ expr stmts) = do
    collectExpr expr
    mapM_ collectStmt stmts
collectStmt (For _ expr1 expr2 stmts) = do
    modify (\(funcs, keywords) -> (funcs, Set.insert "to" keywords))
    collectExpr expr1
    collectExpr expr2
    mapM_ collectStmt stmts
collectStmt (CallStmt functionCall) = collectFunctionCall functionCall
collectStmt (Return expr) =
    collectExpr expr
collectStmt (HashStmt stmnt) =
    collectStmt stmnt
collectStmt (AnnotationStmt _ stmts) =
    mapM_ collectStmt stmts
collectStmt Break =
    modify (\(funcs, keywords) -> (funcs, Set.insert "break" keywords))
collectStmt Continue =
    modify (\(funcs, keywords) -> (funcs, Set.insert "continue" keywords))

collectAssignmentTarget :: AssignmentTarget -> Collector ()
collectAssignmentTarget (ListIndexTarget _ exprs) =
    mapM_ collectExpr exprs
collectAssignmentTarget _ = return ()

collectAssignmentValue :: AssignmentValue -> Collector ()
collectAssignmentValue (ExpressionValue expr) =
    collectExpr expr
collectAssignmentValue _ = return ()

collectMaybeElse :: Maybe Else -> Collector ()
collectMaybeElse (Just (ElseIf expr stmts maybeElse)) = do
    collectExpr expr
    mapM_ collectStmt stmts
    collectMaybeElse maybeElse
collectMaybeElse (Just (Else stmts)) = do
    mapM_ collectStmt stmts
collectMaybeElse Nothing = return ()

-- Expressions and Values

collectExpr :: Expression -> Collector ()
collectExpr (Constant value) = collectValue value
collectExpr (BinaryExp _ expr1 expr2) = do
    collectExpr expr1
    collectExpr expr2
collectExpr (ListIndex _ exprs) =
    mapM_ collectExpr exprs
collectExpr (CallExp functionCall) = collectFunctionCall functionCall
collectExpr (Not expr) = do
    modify (\(funcs, keywords) -> (funcs, Set.insert "not" keywords))
    collectExpr expr
collectExpr _ = return ()

collectValue :: Value -> Collector ()
collectValue Nil =
    modify (\(funcs, keywords) -> (funcs, Set.insert "Nil" keywords))
collectValue (Boolean True) =
    modify (\(funcs, keywords) -> (funcs, Set.insert "true" keywords))
collectValue (Boolean False) =
    modify (\(funcs, keywords) -> (funcs, Set.insert "false" keywords))
collectValue (List exprs) =
    mapM_ collectExpr exprs
collectValue (HashSet set) =
    let exprs = Set.toList set in mapM_ collectExpr exprs
collectValue (HashMap hmap) =
    let pairs = Map.toList hmap in mapM_ collectPair pairs
collectValue _ = return ()

collectPair :: (Expression, Expression) -> Collector ()
collectPair (x, y) = do
    collectExpr x
    collectExpr y

-- Entry point

extractEnv :: Program -> ([String], [String])
extractEnv program =
    let (funcs, keywords) = execState (collectNames program) (Set.empty, Set.empty)
    in (Set.toList funcs, Set.toList keywords)