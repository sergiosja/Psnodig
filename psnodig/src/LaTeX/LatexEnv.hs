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
    mapM_ collectArrays stmts

collectArgs :: FunctionArg -> Collector ()
collectArgs arg = case arg of
    (ArrayArg name _ ) ->
        modify (\(funcs, arrays) -> (funcs, Set.insert name arrays))
    _ -> return ()

collectFunction :: Expression -> Collector ()
collectFunction arg =
    case arg of
        (CallExp (FunctionCall name exps)) -> do
            modify (\(funcs, arrays) -> (Set.insert name funcs, arrays))
            mapM_ collectFunction exps
        _ -> return ()

collectArrays :: Statement -> Collector ()
collectArrays stmt =
    case stmt of
        (Assignment (VariableTarget name) (ArrayExp _)) ->
            modify (\(funcs, arrays) -> (funcs, Set.insert name arrays))
        (Assignment (VariableTarget _) expr) ->
            collectFunction expr
        (CallStmt (FunctionCall name _)) ->
            modify (\(funcs, arrays) -> (Set.insert name funcs, arrays))
        _ -> return ()

extractEnv :: Program -> ([String], [String])
extractEnv program =
    let (funcs, arrays) = execState (collectNames program) (Set.empty, Set.empty)
    in (Set.toList funcs, Set.toList arrays)