module Interpreter(f) where

f :: Int
f = 1

-- module Interpreter (ExecutionState(..), runPsnodig) where

-- import Syntax
-- import Control.Monad.State
-- import Control.Monad.Except
-- import Control.Monad (void, liftM2)
-- import Data.List (find, intercalate)
-- import Data.Either (isRight)
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set

-- type StructDecls = Map.Map String [String]
-- type StructEnv = Map.Map String [(String, Value)]
-- type FuncEnv = [(String, Function)]
-- type Scope = [(String, Value)]
-- type ScopeStack = [Scope]

-- data ExecutionState = ExecutionState {
--     structDecls :: StructDecls,
--     structEnv :: StructEnv,
--     funcEnv :: FuncEnv,
--     scopeStack :: ScopeStack,
--     output :: [String]
-- } deriving (Show)

-- data RuntimeError =
--       VariableNotFound String
--     | FunctionNotFound String
--     | StructNotFound String
--     | ArithmeticError String -- e.g. division by zero and stuff
--     | BadArgument String -- e.g. array["index"]
--     | WrongNumberOfArguments String
--     | NoReturnError String
--     | Error String
--     deriving (Eq, Show)

-- type Psnodig a = StateT ExecutionState (ExceptT RuntimeError IO) a


-- -- Scoping and binding

-- pushScope :: Psnodig ()
-- pushScope = do
--     currScope@(ExecutionState { scopeStack = scopes }) <- get
--     put currScope { scopeStack = ([] : scopes) }

-- popScope :: Psnodig ()
-- popScope = do
--     currScope@(ExecutionState { scopeStack = (_ : scopes) }) <- get
--     put currScope { scopeStack = scopes }

-- lookupVar :: String -> Psnodig (Maybe Value)
-- lookupVar var = do
--     ExecutionState { scopeStack = scopes } <- get
--     return $ searchScopes scopes var
--     where
--         searchScopes [] _ = Nothing
--         searchScopes (scope : rest) variable =
--             case lookup variable scope of
--                 Just v -> Just v
--                 Nothing -> searchScopes rest variable

-- bindVar :: String -> Value -> Psnodig ()
-- bindVar var value = do
--     currScope@(ExecutionState { scopeStack = (top:rest) }) <- get
--     put currScope { scopeStack = ((var, value):top) : rest }

-- bindListVar :: String -> Expression -> Value -> Psnodig ()
-- bindListVar listName indexExpr value = do
--     indexValue <- evalExpr indexExpr
--     maybeList <- lookupVar listName
--     case maybeList of
--         Just (List list) ->
--             case indexValue of
--                 (Number index) ->
--                     case 0 <= index && (fromInteger index) < length list of
--                         True -> do
--                             let updatedList = replaceValueAtIndex list (fromInteger index) value
--                             findAndUpdateScope listName (List updatedList)
--                         False -> throwError $ Error $ "Index " ++ show index ++ " out of bounds of " ++ listName ++ "."
--                 _ -> throwError $ BadArgument $ "List index must be a number."
--         Just _ -> throwError $ BadArgument $ "\"" ++ listName ++ "\" is not a list."
--         Nothing -> throwError $ VariableNotFound $ "No list \"" ++ listName ++ "\" previously defined."

-- findAndUpdateScope :: String -> Value -> Psnodig ()
-- findAndUpdateScope listName newList = do
--     currScope@(ExecutionState { scopeStack = scopes }) <- get
--     case updateScopes scopes listName newList of
--         Just newScopes -> put currScope { scopeStack = newScopes }
--         Nothing -> throwError $ VariableNotFound $ "List " ++ listName ++ " not found." -- should not be possible due to the check in bindListVar

-- updateScopes :: [[(String, Value)]] -> String -> Value -> Maybe [[(String, Value)]]
-- updateScopes [] _ _ = Nothing
-- updateScopes (scope:rest) listName newList =
--     if member listName scope then
--         Just $ (map (updateValue listName newList) scope) : rest
--     else
--         (scope :) <$> updateScopes rest listName newList

-- lookupFunc :: String -> Psnodig (Maybe Function)
-- lookupFunc func = do
--     ExecutionState { funcEnv = env } <- get
--     return $ lookup func env

-- bindFunc :: String -> Function -> Psnodig ()
-- bindFunc name func = do
--     currScope@(ExecutionState { funcEnv = env }) <- get
--     put currScope { funcEnv = (name, func):env }


-- -- Structs

-- processStructDecls :: StructDecl -> Psnodig ()
-- processStructDecls (StructDecl name args) = do
--     currentState <- get
--     let newStructDecls = Map.insert name (map fstArg args) (structDecls currentState)
--     put currentState { structDecls = newStructDecls }

-- lookupStruct :: String -> Psnodig (Maybe Value)
-- lookupStruct var = do
--     ExecutionState { structEnv = env } <- get
--     case Map.lookup var env of
--         Just v -> return $ Just (StructVal (Struct var (map (\p -> (Constant (snd p))) v)))
--         Nothing -> return $ Nothing

-- bindStruct :: String -> Struct -> Psnodig ()
-- bindStruct name (Struct maybeStruct args) = do
--     scope@(ExecutionState { structDecls = decls, structEnv = env }) <- get
--     case Map.lookup maybeStruct decls of
--         Just args' -> do
--             if length args == length args' then do
--                 values <- mapM evalExpr args
--                 let newStructEnv = Map.insert name (zip args' values) env
--                 put scope { structEnv = newStructEnv }
--             else
--                 throwError $ WrongNumberOfArguments $ "Provided " ++ show (length args) ++ " args to a struct that takes " ++ show (length args') ++ " args."
--         Nothing -> throwError $ StructNotFound $ "No struct \"" ++ maybeStruct ++ "\" previously defined."

--     | VariableExp String
--     | ListIndex String Expression -- skulle fikse denne også til | [Expression]
--     | StructFieldExp StructField

-- -- tree.left := 5           (StructField (VariableExp "tree") (VariableExp "left"))
-- -- arr[5].left := 5         (StructField (ListIndex "arr" (Constant (Number 5))) (VariableExp "left"))
-- -- tree.right.left := 5     (StructField (VariableExp "tree") (StructFieldExp (StructField (VariableExp "right") (VariableExp "leff"))))

-- bindStructField :: StructField -> Expression -> Psnodig ()
-- bindStructField (StructField expr1 expr2) valueExpr = do



--     structName <- evalExpr expr1
--     field <- evalExpr expr2
--     scope@(ExecutionState { structEnv = env }) <- get
--     case Map.lookup structName env of
--         Just args -> case lookup field args of
--             Just _ -> do
--                 value <- evalExpr valueExpr
--                 let args' = map (\(x, y) -> if x == field then (x, value) else (x, y)) args
--                 let env' = Map.insert structName args' env
--                 put scope { structEnv = env' }
--             Nothing -> throwError $ BadArgument $ "No field \"" ++ field ++ "\" on struct " ++ structName
--         Nothing -> throwError $ StructNotFound $ "No struct \"" ++ structName ++ "\" previously defined."


-- -- bindStructField (StructField struct field) expr = do
-- --     scope@(ExecutionState { structEnv = env }) <- get
-- --     case Map.lookup struct env of
-- --         Just args -> case lookup field args of
-- --             Just _ -> do
-- --                 value <- evalExpr expr
-- --                 let args' = map (\(x, y) -> if x == field then (x, value) else (x, y)) args
-- --                 let env' = Map.insert struct args' env
-- --                 put scope { structEnv = env' }
-- --             Nothing -> throwError $ BadArgument $ "No field \"" ++ field ++ "\" on struct " ++ struct
-- --         Nothing -> throwError $ StructNotFound $ "No struct \"" ++ struct ++ "\" previously defined."

-- lookupStructField :: ExecutionState -> String -> String -> Maybe Value
-- lookupStructField scope name field = do
--     case Map.lookup name (structEnv scope) of
--         Just fields -> lookup field fields
--         Nothing -> Nothing


-- -- Expressions

-- evalExpr :: Expression -> Psnodig Value
-- evalExpr (Constant v) = return v
-- evalExpr (VariableExp var) = do
--     maybeVar <- lookupVar var
--     case maybeVar of
--         Just val -> return val
--         Nothing -> do
--             maybeStruct <- lookupStruct var
--             case maybeStruct of
--                 Just val -> return val
--                 Nothing -> throwError $ VariableNotFound $ "Variable " ++ var ++ " not found!"
-- evalExpr (BinaryExp op expr1 expr2) = do
--     val1 <- evalExpr expr1
--     val2 <- evalExpr expr2
--     case operate op val1 val2 of
--         Left errMsg -> throwError $ ArithmeticError errMsg
--         Right val -> return val
-- evalExpr (ListIndex var expr) = do
--     maybeList <- lookupVar var
--     idx <- evalExpr expr
--     case maybeList of
--         Just val -> case val of
--             (List list) -> case idx of
--                 (Number n) -> if fromInteger n > length list || fromInteger n < length list
--                     then evalExpr $ list !! fromInteger n else throwError $ BadArgument "List index out of range!"
--                 _ -> throwError $ BadArgument "List index must evaluate to number"
--             _ -> throwError $ BadArgument "Left hand side of expression is not list!"
--         Nothing -> throwError $ VariableNotFound "Variable not found!"
-- evalExpr (CallExp fcall) = callFunction fcall
-- evalExpr (Not expr) = (Boolean . not . bval) <$> evalExpr expr
-- evalExpr (StructFieldExp (StructField name field)) = do
--     scope <- get
--     case lookupStructField scope name field of
--         Just v -> return v
--         Nothing -> throwError $ BadArgument $ "Either struct with name \"" ++ name ++ "\" not declared, or no field \"" ++ field ++ "\" exists."
-- evalExpr (StructExpr struct) =
--     return (StructVal struct)


-- operate :: Operator -> Value -> Value -> Either String Value
-- operate Plus (Number x) (Number y) = return $ Number $ x + y
-- operate Plus (Text s1) (Text s2) = return $ Text $ s1 ++ s2
-- operate Minus (Number x) (Number y) = return $ Number $ x - y
-- operate Times (Number x) (Number y) = return $ Number $ x * y
-- operate Division (Number _) (Number 0) = Left "Division by zero!"
-- -- should probs fix floats/doubles too
-- operate Division (Number x) (Number y) = return $ Number $ div x y
-- operate Modulo (Number _) (Number 0) = Left "Modulo by zero!"
-- operate Modulo (Number x) (Number y) = return $ Number $ mod x y

-- operate LessThan (Number x) (Number y) = return $ Boolean $ x < y
-- operate LessThanEqual (Number x) (Number y) = return $ Boolean $ x <= y
-- operate GreaterThan (Number x) (Number y) = return $ Boolean $ x > y
-- operate GreaterThanEqual (Number x) (Number y) = return $ Boolean $ x >= y
-- operate Equal (Number x) (Number y) = return $ Boolean $ x == y
-- operate NotEqual (Number x) (Number y) = return $ Boolean $ x /= y
-- operate And x y = return $ Boolean $ (bval x) && (bval y)
-- operate Or x y = return $ Boolean $ (bval x) || (bval y)

-- operate _ _ _ = Left "Incompatible operands!"


-- -- Statements

-- evalStmts :: [Statement] -> Psnodig (Either () Value)
-- evalStmts [] = return (Left ())
-- evalStmts ((Return expr):_) = Right <$> evalExpr expr
-- evalStmts (stmt:stmts) = do
--     result <- evalStmt stmt
--     case result of
--         Left _ -> evalStmts stmts
--         Right value -> return (Right value)

-- evalStmt :: Statement -> Psnodig (Either () Value)
-- evalStmt (Assignment assTarget assValue) = do
--     case assValue of
--         (ExpressionValue expr) -> do
--             value <- evalExpr expr
--             case assTarget of
--                 (VariableTarget var) -> do
--                     bindVar var value >> return (Left ())
--                 (ListIndexTarget var indexExpr) -> do
--                     bindListVar var indexExpr value >> return (Left ())
--                 (StructFieldTarget structField) -> do
--                     bindStructField structField expr >> return (Left ())
--         (StructValue struct) ->
--             case assTarget of
--                 (VariableTarget var) ->
--                     bindStruct var struct >> return (Left ())
--                 (ListIndexTarget var expr) ->
--                     bindListVar var expr (StructVal struct) >> return (Left ())
--                 (StructFieldTarget structField) ->
--                     bindStructField structField (StructExpr struct) >> return (Left ())

-- evalStmt (Loop expr stmts) = do
--     val <- evalExpr expr
--     case bval val of
--         True -> do
--             result <- evalStmts stmts
--             case result of
--                 Left _ -> evalStmt (Loop expr stmts)
--                 Right res -> return (Right res)
--         False -> return (Left ())

-- evalStmt (If expr thenStmts maybeElse) = do
--     cond <- evalExpr expr
--     case bval cond of
--         True -> evalStmts thenStmts
--         False -> case maybeElse of
--             Just elseBranch -> evalElse elseBranch
--             Nothing -> return (Left ())

-- evalStmt (ForEach ident expr stmts) = do
--     maybeIterable <- toIterable expr
--     case maybeIterable of
--         Just values -> foldM (executeForEachLoopScope ident stmts) (Left ()) values
--         Nothing -> throwError $ BadArgument "Collection argument is not an iterable. If this is a function call, dereference it before applying as a collection."

-- evalStmt (For ident expr1 expr2 stmts) = do
--     val1 <- evalExpr expr1
--     val2 <- evalExpr expr2
--     case (fromNumber val1, fromNumber val2) of
--         (Just n1, Just n2) -> foldM (executeForLoopScope ident stmts) (Left ()) [n1 .. n2]
--         _ -> throwError $ BadArgument "Range must be whole numbers!"

-- evalStmt (CallStmt f) =
--     callFunction f >> return (Left ())
-- evalStmt (Return expr) =
--     Right <$> evalExpr expr
-- evalStmt (HashStmt stmt) =
--     evalStmt stmt
-- evalStmt (AnnotationStmt _ stmts) =
--     evalStmts stmts

-- -- fix these ?
-- evalStmt (Break) = undefined
-- evalStmt (Continue) = undefined


-- -- Stmt helpers

-- evalElse :: Else -> Psnodig (Either () Value)
-- evalElse (Else stmts) = evalStmts stmts
-- evalElse (ElseIf expr stmts maybeElse) = do
--     cond <- evalExpr expr
--     case bval cond of
--         True -> evalStmts stmts
--         False -> case maybeElse of
--                     Just elseBranch -> evalElse elseBranch
--                     Nothing -> return (Left ())

-- executeForEachLoopScope :: String -> [Statement] -> Either () Value -> Value -> Psnodig (Either () Value)
-- executeForEachLoopScope ident stmts acc value =
--     case acc of
--         Left () -> do
--             pushScope
--             bindVar ident value
--             result <- mapM evalStmt stmts
--             popScope
--             case find isRight result of
--                 Just (Right val) -> return (Right val)
--                 _ -> return (Left ())
--         Right _ -> return acc

-- executeForLoopScope :: String -> [Statement] -> Either () Value -> Integer -> Psnodig (Either () Value)
-- executeForLoopScope ident stmts acc n =
--     case acc of
--         Left () -> do
--             pushScope
--             bindVar ident (Number n)
--             result <- mapM evalStmt stmts
--             popScope
--             case find isRight result of
--                 Just (Right val) -> return (Right val)
--                 _ -> return (Left ())
--         Right _ -> return acc


-- -- Functions

-- -- må legge til feks add to hashmap, add to set osv.
-- -- length og sånt, ceil, floor osv.
-- callFunction :: FunctionCall -> Psnodig Value
-- callFunction (FunctionCall "print" args) = do
--     value <- evalExpr $ head args
--     str <- stringifyValue value False
--     modify (\s -> s { output = output s ++ [str] })
--     return $ Number 1 -- mock value, maybe I should make Void a value type and return that instead. evt endre typen til (Either () Value)

-- callFunction (FunctionCall "length" args) = do
--     maybeList <- evalExpr $ head args
--     case maybeList of
--         (List l) -> return (Number (toInteger $ length l))
--         -- legg til hashmap og sånt og da
--         _ -> throwError $ BadArgument "length can only be called with iterable!"

-- callFunction (FunctionCall name args) = do
--     argsValues <- mapM evalExpr args
--     function <- lookupFunc name
--     case function of
--         Nothing -> throwError $ FunctionNotFound name
--         Just func -> applyFunction func argsValues

-- applyFunction :: Function -> [Value] -> Psnodig Value
-- applyFunction (Function _ args stmts) values = do
--     when (length args /= length values) $ throwError $ WrongNumberOfArguments "Function takes fewer or more args than provided!"
--     pushScope
--     zipWithM_ bindVar (map fstArg args) values
--     result <- evalStmts stmts
--     popScope
--     case result of
--         Left _ -> throwError $ NoReturnError "Function must return something!"
--         Right res -> return res


-- -- Main functions for execution

-- evalProgram :: Program -> Psnodig ()
-- evalProgram (Program structs funcs entryPoint) = do
--     mapM_ processStructDecls structs
--     mapM_ processFunDecl funcs
--     void $ callFunction entryPoint
--   where
--     processFunDecl f@(Function name _ _) = bindFunc name f

-- initialState :: ExecutionState
-- initialState = ExecutionState
--     { structDecls = Map.empty
--     , structEnv = Map.empty
--     , funcEnv = []
--     , scopeStack = []
--     , output = []
--     }

-- runPsnodig :: Program -> IO (Either RuntimeError ExecutionState)
-- runPsnodig program =
--     runExceptT $ execStateT (evalProgram program) initialState


-- -- Helpers

-- fstArg :: Argument -> String
-- fstArg (Argument a _) = a

-- bval :: Value -> Bool
-- bval Nil = False
-- bval (Boolean False) = False
-- bval (Number n) = if n > 0 then True else False
-- bval (Text "") = False
-- bval (List []) = False
-- bval _ = True

-- toIterable :: Expression -> Psnodig (Maybe [Value])
-- toIterable (Constant v) = toIterableValue v
-- toIterable (VariableExp var) = do
--     maybeVar <- lookupVar var
--     case maybeVar of
--         Just v -> toIterableValue v
--         Nothing -> return Nothing
-- toIterable e@(ListIndex _ _) = do
--     v <- evalExpr e
--     toIterableValue v
-- toIterable e@(StructFieldExp _) = do
--     v <- evalExpr e
--     toIterableValue v
-- toIterable _ = return Nothing

-- toIterableValue :: Value -> Psnodig (Maybe [Value])
-- toIterableValue (Text t) = return . Just $ map (Text . return) t
-- toIterableValue (List exprs) = Just <$> mapM evalExpr exprs
-- toIterableValue (HashSet s) = Just <$> mapM evalExpr (Set.toList s)
-- toIterableValue (HashMap m) = Just <$> mapM (\(x, _) -> evalExpr x) (Map.toList m) -- can access values by calling the map with keys!
--                 -- (HashMap m) -> Just <$> mapM (\(x, y) -> liftM2 (,) (evalExpr x) (evalExpr y)) (Map.toList m)
-- toIterableValue _ = return Nothing

-- fromNumber :: Value -> Maybe Integer
-- fromNumber (Number n) = Just n
-- fromNumber _ = Nothing

-- stringifyValue :: Value -> Bool -> Psnodig String
-- stringifyValue v fromList = case v of
--     Text t -> if fromList then return $ '\"' : t ++ "\"" else return t
--     Nil -> return "\"Nil\""
--     Number n ->  return $ show n
--     Boolean b -> return $ show b
--     HashSet s -> return $ show s
--     HashMap m -> return $ show m
--     List l -> stringifyList l
--     StructVal _ -> return "this is a struct :D"

-- stringifyList :: [Expression] -> Psnodig String
-- stringifyList list = do
--     vals <- mapM evalExpr list
--     strList <- mapM (flip stringifyValue True) vals
--     return $ "[" ++ intercalate ", " strList ++ "]"

-- replaceValueAtIndex :: [Expression] -> Int -> Value -> [Expression]
-- replaceValueAtIndex list index newVal =
--     if length list <= index then [(Constant newVal)]
--     else let (before, _ : after) = splitAt index list
--     in before ++ (Constant newVal) : after
--     -- non exhaustive tydeligvis, hva om splitAt index list = (, []). skjønner ikke det helt

-- updateValue :: Eq a => a -> b -> (a, b) -> (a, b)
-- updateValue targetKey newVal (key, val) =
--     if key == targetKey then (key, newVal) else (key, val)

-- member :: String -> [(String, Value)] -> Bool
-- member x = any ((x ==) . fst)


-- -- -- Thought:
-- -- -- There are no global variables
-- -- -- Funcs and structs are global, but variables are always local to their own functions

-- {-
-- func f() {
--     x = 5
--     return g()
-- }

-- func g() {
--     x = 10 -- is the original x overwritten?
--     return x
-- }

-- perhaps we should have a stack of variables
-- all new, local variables are put on top of the stack.
-- when we use a variable, we look from the top and down, until we find a matching one.

-- e.g. [x=5, x=10]
-- when using x, the first one we see is the =10 one

-- and then, when leaving a function, we empty the stack.
-- so we keep a counter as well, on how many elements we've added to the stack
-- a global counter, I guess?
-- at the stard and at the end of the program, it should be 0

-- what about this

-- struct P {
--     age int,
--     friends int
-- }

-- func m() {
--     prsn := struct P(29, 12)
--     f(prsn)
-- }

-- yh okey, only look in struct scope when declaring new structs, but not when looking for variables :)

-- -}


-- -- {- problemet her er at bind egt skal funke for ting som
-- --     x = 5 og h = Height(1, 2, 3)

-- --     når vi har bare struct Height { a int b int c int } etc., så har vi
-- --     ikke egentlig noen 
-- --  -}