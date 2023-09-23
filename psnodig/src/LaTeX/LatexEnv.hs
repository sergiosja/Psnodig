{-# LANGUAGE FlexibleContexts #-}

module LaTeX.LatexEnv (extractEnv) where

-- import Control.Monad.State
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import Data.Maybe (fromMaybe)
-- import Syntax

extractEnv :: Int
extractEnv = 1

-- type Env = Map String [String]

-- initialEnv :: Env
-- initialEnv = Map.fromList [("arrays", []), ("funcs", [])]

-- modifyEnv :: String -> String -> Env -> Env
-- modifyEnv key value env =
--     Map.adjust (value :) key env

-- traverseAST :: Program -> State Env ()
-- traverseAST (Program funcs _) =
--     mapM_ processFunc funcs
--     where
--         processFunc (Function name _ stmts) = do
--             modify $ modifyEnv "funcs" name
--             mapM_ processStmt stmts
--         processStmt (Assignment name (Array _)) =
--             modify $ modifyEnv "arrays" name
--         processStmt _ = return ()

-- extractEnv :: Program -> Env
-- extractEnv program =
--     execState (traverseAST program) initialEnv