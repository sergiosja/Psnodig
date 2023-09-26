module LaTeX.LatexEnv (extractEnv) where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax

type Collector = State (Set String, Set String)

collectNames :: Program -> Collector ()
collectNames (Program funcs _) =
    mapM_ collectFunc funcs

collectFunc :: Function -> Collector ()
collectFunc (Function name _ stmts) = do
    modify (\(funcs, arrays) -> (Set.insert name funcs, arrays))
    mapM_ collectArray stmts

collectArray :: Statement -> Collector ()
collectArray (Assignment (VariableTarget name) (ArrayExp _)) = 
    modify (\(funcs, arrays) -> (funcs, Set.insert name arrays))
collectArray _ = return ()

extractEnv :: Program -> ([String], [String])
extractEnv program =
    let (funcs, arrays) = execState (collectNames program) (Set.empty, Set.empty)
    in (Set.toList funcs, Set.toList arrays)