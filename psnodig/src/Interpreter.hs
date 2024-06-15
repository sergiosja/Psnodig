module Interpreter
    ( ExecutionState(..)
    , RuntimeError(..)
    , runPsnodig
    , evalStmt
    , evalExpr
    , processStructDecls
    , stringifyValue
    , bval
    , callFunction
    ) where

import Syntax
import Control.Monad.State
import Control.Monad.Except
import Data.List (find, intercalate)
import Data.Either (isRight)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

type StructDecls = Map.Map String [String]
type FuncDecls = Map.Map String FunctionDecl
type Scope = [[(String, Value)]]
type ScopeStack = [Scope]

-- type Binding = (String, Value)
-- type BindingStack = [Binding]
-- type ScopeStack = [BindingStack]
-- type FunctionStack = [ScopeStack]

data ExecutionState = ExecutionState
    { structDecls :: StructDecls
    , funcDecls   :: FuncDecls
    , scopeStack  :: ScopeStack
    , output      :: [String]
    }
    deriving (Show, Eq)

data RuntimeError =
      VariableNotFound String
    | OutOfBounds String
    | FunctionNotFound String
    | StructNotFound String
    | ArithmeticError String
    | ListNotFound String
    | BadArgument String
    | InvalidStructField String
    | NoImplementationError String
    | WrongNumberOfArguments String
    | NoReturnError String
    | MissingEntryPoint String
    | RuntimeErrorWithOutput [String] RuntimeError
    deriving (Show, Eq)

type Psnodig = StateT ExecutionState (ExceptT RuntimeError IO)


-- Scoping and binding

-- "Outer layer", the scopes of active functions
pushFunctionScope :: Psnodig ()
pushFunctionScope = do
    currScope@(ExecutionState { scopeStack = scopes }) <- get
    put currScope { scopeStack = ([] : scopes) }

popFunctionScope :: Psnodig ()
popFunctionScope = do
    currScope@(ExecutionState { scopeStack = (_ : scopes) }) <- get
    put currScope { scopeStack = scopes }

-- "Inner layer", the scopes of active variables within functions
pushScope :: Psnodig ()
pushScope = do
    currScope@(ExecutionState { scopeStack = (h : rest) }) <- get
    put currScope { scopeStack = (([] : h) : rest) }

popScope :: Psnodig ()
popScope = do
    currScope@(ExecutionState { scopeStack = ((_ : h) : rest) }) <- get
    put currScope { scopeStack = h : rest }


lookupVar :: String -> Psnodig (Maybe Value)
lookupVar var = do
    ExecutionState { scopeStack = (scopes : _) } <- get
    return $ searchScopes scopes var
    where
        searchScopes [] _ = Nothing
        searchScopes (scope : rest) variable =
            case lookup variable scope of
                Just v -> Just v
                Nothing -> searchScopes rest variable

bindVar :: String -> Value -> Psnodig ()
bindVar var value = do
    currScope@(ExecutionState { scopeStack = (currentFuncScope : rest) }) <- get
    put currScope { scopeStack = (updateVar currentFuncScope var value) : rest }

updateVar :: [[(String, Value)]] -> String -> Value -> [[(String, Value)]]
updateVar [] var value = [[(var, value)]]
updateVar scopes@(scope : _) var value =
    let updatedScopes@(currScope : restScopes) = updateAllScopes scopes var value
    in if varInScope var scope then updatedScopes else (((var, value) : currScope) : restScopes)

updateAllScopes :: [[(String, Value)]] -> String -> Value -> [[(String, Value)]]
updateAllScopes scopes var val = map (updateCurrentScope var val) scopes

updateCurrentScope :: String -> Value -> [(String, Value)] -> [(String, Value)]
updateCurrentScope var val =
    map (\orig@(var', _) -> if var' == var then (var', val) else orig)

varInScope :: String -> [(String, Value)] -> Bool
varInScope var scope = any (\(var', _) -> var' == var) scope


updateListVar :: String -> [Expression] -> Value -> Psnodig ()
updateListVar listName indexExprs value = do
    maybeList <- lookupVar listName
    case maybeList of
        Just (List list) -> do
            list' <- updateNestedListVar (List list) indexExprs value
            findAndUpdateScope listName list'
        Just _ -> throwError' (ListNotFound $ "\"" ++ listName ++ "\" is not a list.")
        Nothing -> throwError' (VariableNotFound $ "No list \"" ++ listName ++ "\" previously defined.")

updateNestedListVar :: Value -> [Expression] -> Value -> Psnodig Value
updateNestedListVar (List list) (indexExpr : []) value = do
    maybeIndex <- evalExpr indexExpr
    case maybeIndex of
        Number index ->
            if 0 <= index && (fromInteger index) < length list
            then return $ List $ replaceValueAtIndex list (fromInteger index) value
            else throwError' (OutOfBounds $ "Index " ++ show index ++ " out of bounds.")
        _ -> throwError' (BadArgument $ "List index must be a number.")

updateNestedListVar (List list) (indexExpr : rest) value = do
    maybeIndex <- evalExpr indexExpr
    case maybeIndex of
        Number index ->
            if 0 <= index && (fromInteger index) < length list
            then do
                innerList <- evalExpr $ list !! (fromInteger index)
                (List list') <- updateNestedListVar innerList rest value
                return $ List $ replaceValueAtIndex list (fromInteger index) (List list')                
            else throwError' (OutOfBounds $ "Index " ++ show index ++ " out of bounds.")
        _ -> throwError' (BadArgument $ "List index must be a number.")

updateNestedListVar _ _ _ = throwError' (ListNotFound "Tried to update a list at an index, but provided argument is not a list.")

findAndUpdateScope :: String -> Value -> Psnodig ()
findAndUpdateScope listName newList = do
    currScope@(ExecutionState { scopeStack = (scopes : rest) }) <- get
    case updateScopes scopes listName newList of
        Just newScopes -> put currScope { scopeStack = (newScopes : rest) }
        Nothing -> throwError' (ListNotFound $ "List " ++ listName ++ " not found.")

updateScopes :: [[(String, Value)]] -> String -> Value -> Maybe [[(String, Value)]]
updateScopes [] _ _ = Nothing
updateScopes (scope:rest) listName newList =
    if member listName scope then
        Just $ (map (updateValue listName newList) scope) : rest
    else
        (scope :) <$> updateScopes rest listName newList

lookupFunc :: String -> Psnodig (Maybe FunctionDecl)
lookupFunc funcName = do
    ExecutionState { funcDecls = env } <- get
    return $ Map.lookup funcName env

bindFunc :: String -> FunctionDecl -> Psnodig ()
bindFunc name func = do
    env <- get
    let newFuncEnv = Map.insert name func (funcDecls env)
    put env { funcDecls = newFuncEnv }


-- Structs

lookupStructDecl :: String -> Psnodig (Maybe [String])
lookupStructDecl var = do
    ExecutionState { structDecls = env } <- get
    case Map.lookup var env of
        Just fields -> return $ Just fields
        Nothing -> return $ Nothing

processStructDecls :: StructDecl -> Psnodig ()
processStructDecls (StructDecl name args) = do
    env <- get
    let newStructDecls = Map.insert name (map fstArg args) (structDecls env)
    put env { structDecls = newStructDecls }

bindStruct :: String -> Struct -> Psnodig ()
bindStruct name (Struct maybeStruct args) = do
    (ExecutionState { structDecls = decls }) <- get
    case Map.lookup maybeStruct decls of
        Just args' -> do
            if length args == length args' then do
                values <- mapM evalExpr args
                bindVar name (StructVal $ zip args' values)
            else
                throwError' (WrongNumberOfArguments $ "Provided " ++ show (length args) ++ " args to a struct that takes " ++ show (length args') ++ " args.")
        Nothing -> throwError' (StructNotFound $ "No struct \"" ++ maybeStruct ++ "\" previously defined.")

updateStruct :: String -> [(String, Value)] -> Psnodig ()
updateStruct name fields =
    bindVar name (StructVal fields)

updateStructField :: StructField -> Expression -> Psnodig ()
updateStructField (StructField structName fieldExpr) valueExpr = do
    case structName of
        VariableExp name -> do
            maybeStruct <- lookupVar name
            case maybeStruct of
                Just (StructVal fields) -> do
                    StructVal newEnv <- updateField fields fieldExpr valueExpr
                    updateStruct name newEnv
                _ -> throwError' (StructNotFound $ name ++ " is not a defined struct.")
        listIndex@(ListIndex listName indexExprs) -> do
            maybeStruct <- evalExpr listIndex
            case maybeStruct of
                StructVal fields -> do
                    StructVal newEnv <- updateField fields fieldExpr valueExpr
                    updateListVar listName indexExprs (StructVal newEnv)
                _ -> throwError' (StructNotFound $ listName ++ " does not contain a struct at given index.")
        _ -> throwError' (BadArgument "Attempted to update struct field, but provided argument is not a defined struct.")

updateField :: [(String, Value)] -> Expression -> Expression -> Psnodig Value
updateField env fieldExpr valueExpr = do
    case fieldExpr of
        VariableExp name -> do
            newVal <- evalExpr valueExpr
            updateListEntry env name newVal
        (ListIndex listName indexExprs) ->
            case lookup listName env of
                Just list@(List _) -> do
                    list' <- updateSingleFieldList list indexExprs valueExpr
                    return $ StructVal [(if listName == arg then (arg, list') else (arg, val)) | (arg, val) <- env]
                _ -> throwError' (ListNotFound $ "Tried to update " ++ listName ++ ", but argument is not a list.")
        (StructFieldExp (StructField field fieldExpr')) ->
            case field of
                VariableExp name -> do
                    case lookup name env of
                        Just (StructVal fields) -> do
                            fields' <- updateField fields fieldExpr' valueExpr
                            updateListEntry env name fields'
                        _ -> throwError' (InvalidStructField $ name ++ " does not contain struct fields.")
                (ListIndex listName indexExprs) -> do
                    case lookup listName env of
                        Just list@(List _) -> do
                            list' <- updateRecursiveFieldList list indexExprs fieldExpr' valueExpr
                            updateListEntry env listName list'
                        _ -> throwError' (ListNotFound $ listName ++ " is not a list.")
                _ -> throwError' (BadArgument $ show field ++ " is not a valid field.")
                        
        _ -> throwError' (BadArgument "Cannot update provided field.")


updateSingleFieldList :: Value -> [Expression] -> Expression -> Psnodig Value
updateSingleFieldList (List list) (indexExpr : []) valueExpr = do
    maybeIndex <- evalExpr indexExpr
    value <- evalExpr valueExpr
    case maybeIndex of
        Number index ->
            if 0 <= index && (fromInteger index) < length list
            then return $ List (replaceValueAtIndex list (fromInteger index) value)
            else throwError' (OutOfBounds "Tried to update list, but index is out of bounds.")
        _ -> throwError' (BadArgument "Tried to update list, but provided index is not a number.")

updateSingleFieldList (List list) (indexExpr : rest) valueExpr = do
    maybeIndex <- evalExpr indexExpr
    case maybeIndex of
        Number index ->
            if 0 <= index && (fromInteger index) < length list
            then do
                let listExpr = list !! (fromInteger index)
                list' <- evalExpr listExpr
                list'' <- updateSingleFieldList list' rest valueExpr
                return $ List (replaceValueAtIndex list (fromInteger index) list'')
            else throwError' (OutOfBounds "Tried to update list, but index is out of bounds.")
        _ -> throwError' (BadArgument "Tried to update list, but provided index is not a number.")

updateSingleFieldList _ _ _ = throwError' (ListNotFound "Tried to update list index value, but provided argument is not a list.")


updateRecursiveFieldList :: Value -> [Expression] -> Expression -> Expression -> Psnodig Value
updateRecursiveFieldList (List list) (indexExpr : []) fieldExpr valueExpr = do
    maybeIndex <- evalExpr indexExpr
    case maybeIndex of
        Number index ->
            if 0 <= index && (fromInteger index) < length list
            then do
                StructVal fields <- evalExpr $ list !! (fromInteger index)
                list' <- updateField fields fieldExpr valueExpr
                return $ List (replaceValueAtIndex list (fromInteger index) list')
            else throwError' (OutOfBounds "Tried to update list, but index is out of bounds.")
        _ -> throwError' (BadArgument "Tried to update list, but provided index is not a number.")

updateRecursiveFieldList (List list) (indexExpr : rest) fieldExpr valueExpr = do
    maybeIndex <- evalExpr indexExpr
    case maybeIndex of
        Number index ->
            if 0 <= index && (fromInteger index) < length list
            then do
                let listExpr = list !! (fromInteger index)
                list' <- evalExpr listExpr
                list'' <- updateRecursiveFieldList list' rest fieldExpr valueExpr
                return $ List (replaceValueAtIndex list (fromInteger index) list'')
            else throwError' (OutOfBounds "Tried to update list, but index is out of bounds.")
        _ -> throwError' (BadArgument "Tried to update list, but provided index is not a number.")

updateRecursiveFieldList _ _ _ _ = throwError' (ListNotFound "Tried to update list index value, but provided argument is not a list.")


updateListEntry :: [(String, Value)] -> String -> Value -> Psnodig Value
updateListEntry env name newVal = return $ StructVal [(if name == arg then (arg, newVal) else (arg, val)) | (arg, val) <- env]

-- Values

evalValue :: Value -> Psnodig Value
evalValue (List list) = do
    vals <- mapM evalExpr list
    return $ List (map (\v -> Constant v) vals)
evalValue (HashSet hs) = do
    vals <- mapM evalExpr (Set.toList hs)
    return $ HashSet (Set.fromList (map (\v -> Constant v) vals))
evalValue (HashMap hm) = do
    vals <- mapM evalPair (Map.toList hm)
    return $ HashMap (Map.fromList (map (\(k, v) -> (Constant k, Constant v)) vals))
evalValue v = return v

evalPair :: (Expression, Expression) -> Psnodig (Value, Value)
evalPair (x, y) = do
    x' <- evalExpr x
    y' <- evalExpr y
    return (x', y')


-- Expressions

evalExpr :: Expression -> Psnodig Value
evalExpr (Constant v) = evalValue v
evalExpr (VariableExp var) = do
    maybeVar <- lookupVar var
    case maybeVar of
        Just val -> return val
        Nothing -> throwError' (VariableNotFound $ "Variable " ++ var ++ " not found.")
evalExpr (BinaryExp op expr1 expr2) = do
    res <- operate op expr1 expr2
    case res of
        Left errMsg -> throwError' (ArithmeticError errMsg)
        Right val -> return val
evalExpr (ListIndex var indexExprs) = do
    maybeList <- lookupVar var
    case maybeList of
        Just val -> evalNestedIndex val indexExprs
        Nothing  -> throwError' (ListNotFound $ "List " ++ var ++ " not found.")
evalExpr (CallExp fcall) = callFunction fcall
evalExpr (Not expr) = (Boolean . not . bval) <$> evalExpr expr
evalExpr (StructFieldExp structField) = evalStructFieldExprMain structField
evalExpr (StructExpr (Struct maybeStruct exprs)) = do
    maybeStructDecl <- lookupStructDecl maybeStruct
    case maybeStructDecl of
        Just fields ->
            if length fields == length exprs
            then do
                values <- mapM evalExpr exprs
                return (StructVal $ zip fields values)
            else throwError' (WrongNumberOfArguments $ "Incorrect number of arguments provided for struct " ++ maybeStruct ++ ".")
        Nothing -> throwError' (StructNotFound $ "Struct " ++ maybeStruct ++ " is not defined.")

evalStructFieldExprMain :: StructField -> Psnodig Value
evalStructFieldExprMain (StructField structName fieldExpr) =
    case structName of
        VariableExp name -> do
            maybeStruct <- lookupVar name
            case maybeStruct of
                Just (StructVal fields) -> evalStructFieldExprRec fields fieldExpr
                _ -> throwError' (StructNotFound $ name ++ " is not a defined struct.")
        listIndex@(ListIndex listName _) -> do
            maybeStruct <- evalExpr listIndex
            case maybeStruct of
                StructVal fields -> evalStructFieldExprRec fields fieldExpr
                _ -> throwError' (StructNotFound $ listName ++ " does not contain a struct at given index.")
        _ -> throwError' (StructNotFound $ "Cannot lookup struct field(s) of " ++ show structName ++ ".")

evalStructFieldExprRec :: [(String, Value)] -> Expression -> Psnodig Value
evalStructFieldExprRec fields expr = do
    case expr of
        VariableExp name ->
            case lookup name fields of
                Just v -> return v
                Nothing -> throwError' (StructNotFound $ name ++ " is not a valid struct.")
        (ListIndex name indexExprs) ->
            case lookup name fields of
                Just list@(List _) -> evalNestedIndex list indexExprs
                _ -> throwError' (StructNotFound $ name ++ " is not a valid struct.")
        (StructFieldExp (StructField field expr')) ->
            case field of
                VariableExp name -> do
                    case lookup name fields of
                        Just (StructVal fields') -> evalStructFieldExprRec fields' expr'
                        _ -> throwError' (InvalidStructField $ name ++ " field does not exist on provided struct.")
                (ListIndex name indexExprs) ->
                    case lookup name fields of
                        Just list@(List _) -> do
                            StructVal fields' <- evalNestedIndex list indexExprs
                            evalStructFieldExprRec fields' expr'
                        _ -> throwError' (InvalidStructField $ name ++ " field does not exist on provided struct.")
                _ -> throwError' (BadArgument $ show field ++ " is an invalid struct field.")
        _ -> throwError' (StructNotFound $ "Provided argument is not a struct. Make sure to wrap structfield in parentheses.")


-- Operators

operate :: Operator -> Expression -> Expression -> Psnodig (Either String Value)
operate And expr1 expr2 = do
    val1 <- evalExpr expr1
    if not $ bval val1
    then return . Right $ Boolean False
    else do
        val2 <- evalExpr expr2
        return $ Right $ Boolean (bval val2)
operate Or expr1 expr2 = do
    val1 <- evalExpr expr1
    if bval val1 then return $ Right $ Boolean True
    else do
            val2 <- evalExpr expr2
            return $ Right $ Boolean (bval val2)
operate op expr1 expr2 = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    return $ operate' op val1 val2

operate' :: Operator -> Value -> Value -> Either String Value
operate' Plus (Number x) (Number y) = Right $ Number $ x + y
operate' Plus (Decimal x) (Number y) = Right $ Decimal $ x + (fromInteger y)
operate' Plus (Number x) (Decimal y) = Right $ Decimal $ (fromInteger x) + y
operate' Plus (Text s1) (Text s2) = Right $ Text $ s1 ++ s2

operate' Minus (Number x) (Number y) = Right $ Number $ x - y
operate' Minus (Decimal x) (Number y) = Right $ Decimal $ x - (fromInteger y)
operate' Minus (Number x) (Decimal y) = Right $ Decimal $ (fromInteger x) - y

operate' Times (Number x) (Number y) = Right $ Number $ x * y
operate' Times (Decimal x) (Number y) = Right $ Decimal $ x * (fromInteger y)
operate' Times (Number x) (Decimal y) = Right $ Decimal $ (fromInteger x) * y

operate' Division _ (Number 0) = Left "Division by zero!"
operate' Division _ (Decimal 0.0) = Left "Division by zero!"
operate' Division (Number x) (Number y) = Right $ Number $ div x y
operate' Division (Decimal x) (Number y) = Right $ Decimal $ x / (fromInteger y)
operate' Division (Number x) (Decimal y) = Right $ Decimal $ (fromInteger x) / y

operate' Modulo _ (Number 0) = Left "Modulo by zero!"
operate' Modulo (Number x) (Number y) = Right $ Number $ mod x y
operate' Modulo (Decimal _) _ = Left "Avoid using modulo with decimals!"
operate' Modulo _ (Decimal _) = Left "Avoid using modulo with decimals!"

operate' LessThan (Number x) (Number y) = Right $ Boolean $ x < y
operate' LessThan (Decimal x) (Number y) = Right $ Boolean $ x < (fromInteger y)
operate' LessThan (Number x) (Decimal y) = Right $ Boolean $ (fromInteger x) < y

operate' LessThanEqual (Number x) (Number y) = Right $ Boolean $ x <= y
operate' LessThanEqual (Decimal x) (Number y) = Right $ Boolean $ x <= (fromInteger y)
operate' LessThanEqual (Number x) (Decimal y) = Right $ Boolean $ (fromInteger x) <= y

operate' GreaterThan (Number x) (Number y) = Right $ Boolean $ x > y
operate' GreaterThan (Decimal x) (Number y) = Right $ Boolean $ x > (fromInteger y)
operate' GreaterThan (Number x) (Decimal y) = Right $ Boolean $ (fromInteger x) > y

operate' GreaterThanEqual (Number x) (Number y) = Right $ Boolean $ x >= y
operate' GreaterThanEqual (Decimal x) (Number y) = Right $ Boolean $ x >= (fromInteger y)
operate' GreaterThanEqual (Number x) (Decimal y) = Right $ Boolean $ (fromInteger x) >= y

operate' Equal (Number x) (Number y) = Right $ Boolean $ x == y
operate' Equal (Decimal x) (Number y) = Right $ Boolean $ x == (fromInteger y)
operate' Equal (Number x) (Decimal y) = Right $ Boolean $ (fromInteger x) == y
operate' Equal Nil Nil = Right $ Boolean True
operate' Equal (Text t1) (Text t2) = Right $ Boolean $ t1 == t2
operate' Equal (Boolean x) (Boolean y) = Right $ Boolean $ x == y
operate' Equal _ _ = Right $ Boolean False

operate' NotEqual (Number x) (Number y) = Right $ Boolean $ x /= y
operate' NotEqual (Decimal x) (Number y) = Right $ Boolean $ x /= (fromInteger y)
operate' NotEqual (Number x) (Decimal y) = Right $ Boolean $ (fromInteger x) /= y
operate' NotEqual Nil Nil = Right $ Boolean False
operate' NotEqual (Text t1) (Text t2) = Right $ Boolean $ t1 /= t2
operate' NotEqual (Boolean x) (Boolean y) = Right $ Boolean $ x /= y
operate' NotEqual _ _ = Right $ Boolean False

operate' op x y = Left $ "Incompatible operands! Tried to apply " ++
    (show op) ++ " to " ++ (show x) ++ " and " ++ (show y) ++ "!"


-- Statements

evalStmts :: [Statement] -> Psnodig (Either () Value)
evalStmts [] = return (Left ())
evalStmts ((Return expr):_) = Right <$> evalExpr expr
evalStmts (stmt:stmts) = do
    result <- evalStmt stmt
    case result of
        Left _ -> evalStmts stmts
        Right value -> return (Right value)

evalStmt :: Statement -> Psnodig (Either () Value)
evalStmt (Assignment assVar assExpr) =
    case assExpr of -- burde egt ikke være to sjekker her. bedre å ha en sjekk i bindVar. fordi feks bindStruct går dit uansett?
        (StructExpr struct) ->
            case assVar of
                (VariableTarget var) ->
                    bindStruct var struct >> return (Left ())
                (ListIndexTarget var indexExprs) -> do
                    fields <- evalExpr (StructExpr struct)
                    updateListVar var indexExprs fields >> return (Left ())
                (StructFieldTarget structField) ->
                    updateStructField structField (StructExpr struct) >> return (Left ())
        _ -> do
            value <- evalExpr assExpr
            case assVar of
                (VariableTarget var) ->
                    bindVar var value >> return (Left ())
                (ListIndexTarget var indexExprs) ->
                    updateListVar var indexExprs value >> return (Left ())
                (StructFieldTarget structField) ->
                    updateStructField structField assExpr >> return (Left ())

evalStmt (Loop expr stmts) = do
    val <- evalExpr expr
    case bval val of
        True -> do
            result <- evalStmts stmts
            case result of
                Left _ -> evalStmt (Loop expr stmts)
                Right res -> return (Right res)
        False -> return (Left ())

evalStmt (If expr thenStmts maybeElse) = do
    cond <- evalExpr expr
    case bval cond of
        True -> evalStmts thenStmts
        False -> case maybeElse of
            Just elseBranch -> evalElse elseBranch
            Nothing -> return (Left ())

evalStmt (ForEach ident expr stmts) = do
    maybeIterable <- evalExpr expr >>= toIterable
    case maybeIterable of
        Just values -> foldM (executeForEachLoopScope ident stmts) (Left ()) values
        Nothing -> throwError' (BadArgument "Collection argument is not an iterable. If this is a function call, dereference it before applying as a collection.")

evalStmt (For ident expr1 expr2 stmts) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case (fromNumber val1, fromNumber val2) of
        (Just n1, Just n2) ->
            foldM (executeForLoopScope ident stmts) (Left ()) (forRange n1 n2)
        _ -> throwError' (BadArgument "Provided arguments in for-loop must be whole numbers.")

evalStmt (CallStmt f) =
    callFunction f >> return (Left ())
evalStmt (Return expr) =
    Right <$> evalExpr expr
evalStmt (HashStmt stmt) =
    evalStmt stmt
evalStmt (AnnotationStmt _ stmts) =
    evalStmts stmts

evalStmt Break = throwError' (NoImplementationError "No implementation for `Break`. Avoid usage when executing programs.")
evalStmt Continue = throwError' (NoImplementationError "No implementation for `Continue`. Avoid usage when executing programs.")


evalElse :: Else -> Psnodig (Either () Value)
evalElse (Else stmts) = evalStmts stmts
evalElse (ElseIf expr stmts maybeElse) = do
    cond <- evalExpr expr
    case bval cond of
        True -> evalStmts stmts
        False -> case maybeElse of
                    Just elseBranch -> evalElse elseBranch
                    Nothing -> return (Left ())

executeForEachLoopScope :: String -> [Statement] -> Either () Value -> Value -> Psnodig (Either () Value)
executeForEachLoopScope ident stmts acc value =
    case acc of
        Left () -> do
            pushScope
            bindVar ident value
            result <- mapM evalStmt stmts
            popScope
            case find isRight result of
                Just (Right val) -> return (Right val)
                _ -> return (Left ())
        Right _ -> return acc

executeForLoopScope :: String -> [Statement] -> Either () Value -> Integer -> Psnodig (Either () Value)
executeForLoopScope ident stmts acc n =
    case acc of
        Left () -> do
            pushScope
            bindVar ident (Number n)
            result <- mapM evalStmt stmts
            popScope
            case find isRight result of
                Just (Right val) -> return (Right val)
                _ -> return (Left ())
        Right _ -> return acc

forRange :: Integer -> Integer -> [Integer]
forRange x y = if x <= y then [x..y] else [x,x-1..y]

-- Functions

callFunction :: FunctionCall -> Psnodig Value
callFunction (FunctionCall "floor" args) = do
    when (length args /= 1)
        $ throwError' (WrongNumberOfArguments "Function 'floor' takes 1 argument: floor( numeric ).")
    value <- evalExpr $ head args
    case value of
        Number _ -> return $ value
        Decimal d -> return . Number $ floor d
        _ -> throwError' (BadArgument "Function 'floor' can only be applied to numeric values.")

callFunction (FunctionCall "ceil" args) = do
    when (length args /= 1)
        $ throwError' (WrongNumberOfArguments "Function 'ceil' takes 1 argument: ceil( numeric ).")
    value <- evalExpr $ head args
    case value of
        Number _ -> return $ value
        Decimal d -> return . Number $ ceiling d
        _ -> throwError' (BadArgument "Function 'ceil' can only be applied to numeric values.")

callFunction (FunctionCall "min" args) = do
    values <- mapM evalExpr args
    unless (all (\v -> isNumber v || isDecimal v) values) $
        throwError' (BadArgument "All arguments to function 'min' must be numeric values.")
    
    let numberList = mapMaybe fromNumber values
    let decimalList = mapMaybe fromDecimal values

    case (numberList, decimalList) of
        ([], []) -> throwError' (BadArgument "Cannot call `min` with no arguments.")
        (_, []) -> return . Number $ minimum numberList
        ([], _) -> return . Decimal $ minimum decimalList
        (_, _) -> return $
            let mNum = minimum numberList
                mDec = minimum decimalList
            in if (fromInteger mNum) < mDec
                then Number mNum else Decimal mDec

callFunction (FunctionCall "max" args) = do
    values <- mapM evalExpr args
    unless (all (\v -> isNumber v || isDecimal v) values) $
        throwError' (BadArgument "All arguments to function 'max' must be numeric values.")

    let numberList = mapMaybe fromNumber values
    let decimalList = mapMaybe fromDecimal values

    case (numberList, decimalList) of
        ([], []) -> throwError' (BadArgument "Cannot call `max` with no arguments.")
        (_, []) -> return . Number $ maximum numberList
        ([], _) -> return . Decimal $ maximum decimalList
        (_, _) -> return $
            let mNum = maximum numberList
                mDec = maximum decimalList
            in if (fromInteger mNum) > mDec
                then Number mNum else Decimal mDec

callFunction (FunctionCall "get" args) = do
    when (length args /= 2)
        $ throwError' (WrongNumberOfArguments "Function 'get' takes 2 arguments: get( key , map ).")
    let mapExpr = head $ tail args
    case mapExpr of
        VariableExp mapName -> do
            maybeMap <- evalExpr mapExpr
            case maybeMap of
                HashMap m -> do
                    case Map.lookup (head args) m of
                        Just v -> evalExpr v
                        Nothing -> do
                            var <- evalExpr (head args)
                            case Map.lookup (Constant var) m of
                                Just v -> evalExpr v
                                Nothing -> throwError' (BadArgument $ "Function 'get' takes two arguments: get( key, map ). " ++ (show $ head args) ++ " is likely an invalid key.")
                _ -> throwError' (BadArgument $ "Function 'get' takes two arguments: get( key, map ). " ++ mapName ++ " is likely an invalid Hashmap.")
        _ -> throwError' (BadArgument "Function 'get' takes two arguments: get( key, map ).")

callFunction (FunctionCall "add" args) = do
    when (length args < 2 || length args > 3)
        $ throwError' (WrongNumberOfArguments "Function 'add' takes either 2 arguments: add( value , set ) or 3 arguments: add( key, value, map ).")
    if length args == 2
    then do
        let VariableExp setName = args !! 1
        maybeSet <- evalExpr (VariableExp setName)
        case maybeSet of
            HashSet hs -> do
                newVal <- evalExpr (head args)
                let newSet = HashSet $ Set.insert (Constant newVal) hs
                findAndUpdateScope setName newSet
                return $ Number 1
            _ -> throwError' (BadArgument "Function 'add'. Second argument is likely not a HashSet.")
    else do
        let VariableExp mapName = args !! 2
        maybeMap <- evalExpr (VariableExp mapName)
        case maybeMap of
            HashMap hm -> do
                newKey <- evalExpr (head args)
                newVal <- evalExpr (head (tail args))

                let newMap = HashMap $ Map.insert (Constant newKey) (Constant newVal) hm
                findAndUpdateScope mapName newMap
                return $ Number 1
            _ -> throwError' (BadArgument "Function 'add'. Third argument is likely not a HashMap.")

callFunction (FunctionCall "in" args) = do
    when (length args /= 2)
        $ throwError' (WrongNumberOfArguments "Function 'in' takes 2 arguments: add( value , list/set/map ).")
    let target = head args
    maybeCollection <- evalExpr (head $ tail args)
    case maybeCollection of
        List l -> do
            targetValue <- evalExpr target
            return $ Boolean $ elem (Constant targetValue) l
        HashSet hs -> do
            targetValue <- evalExpr target
            return $ Boolean $ Set.member (Constant targetValue) hs
        HashMap hm -> do
            targetValue <- evalExpr target
            return $ Boolean $ case Map.lookup (Constant targetValue) hm of
                Just _ -> True
                Nothing -> False
        _ -> throwError' (BadArgument "Second argument of function `in` must be of type List, HashSet, or HashMap.")

callFunction (FunctionCall "pop" args) = do
    when (length args /= 1)
        $ throwError' (WrongNumberOfArguments "Function 'pop' takes 1 argument: pop( list ).")
    maybeList <- evalExpr (head args)
    case maybeList of
        List list ->
            if null list
            then throwError' (BadArgument $ "Function `pop` takes non-empty list as argument. Make sure the provided list is not empty.")
            else do
                lastValue <- evalExpr (last list)
                case head args of
                    VariableExp listName -> do
                        let newList = List $ init list
                        findAndUpdateScope listName newList
                        return lastValue
                    ListIndex listName indexes -> do
                        let newList = List $ init list
                        updateListVar listName indexes newList
                        return lastValue
                    StructFieldExp s@(StructField _ _) -> do
                        let newList = Constant (List $ init list)
                        updateStructField s newList
                        return lastValue
                    _ -> throwError' (BadArgument $ (show $ head args) ++ " is not a valid list.")
        _ -> throwError' (BadArgument $ "Function `pop` takes a list as argument. " ++ show (head args) ++ " is possibly not a list.")

callFunction (FunctionCall "append" args) = do
    when (length args /= 2)
        $ throwError' (WrongNumberOfArguments "Function 'append' takes 2 arguments: append( value , list ).")
    let targetListExpr = args !! 1
    maybeList <- evalExpr targetListExpr
    case maybeList of
        List list ->
            case targetListExpr of
                VariableExp listName -> do
                    newValue <- evalExpr (head args)
                    let newList = List $ list ++ [(Constant newValue)]
                    findAndUpdateScope listName newList
                    return $ Number 1 -- burde egt returnere listen?
                ListIndex listName indexes -> do
                    newValue <- evalExpr (head args)
                    let newList = List $ list ++ [(Constant newValue)]
                    updateListVar listName indexes newList
                    return $ Number 1 -- burde egt returnere listen?
                StructFieldExp s@(StructField _ _) -> do
                    newValue <- evalExpr (head args)
                    let newList = Constant (List $ list ++ [(Constant newValue)])
                    updateStructField s newList
                    return $ Number 1
                _ -> throwError' (BadArgument $ "Function 'append' takes two arguments: append( value , list ). The second argument is likely an invalid list.")
        _ -> throwError' (BadArgument $ "Function 'append' takes two arguments: append( value , list ). The second argument is possibly not a list.")

callFunction (FunctionCall "print" args) = do
    values <- mapM evalExpr args
    strings <- mapM (\v -> stringifyValue v False) values
    let string = intercalate " " strings
    modify (\s -> s { output = output s ++ [string] })
    return . Number . toInteger . length $ args

callFunction (FunctionCall "length" args) = do
    maybeIterable <- evalExpr $ head args
    case maybeIterable of
        (Text t) -> toNum t
        (List l) -> toNum l
        (HashSet s) -> toNum $ Set.toList s
        (HashMap m) -> toNum $ Map.toList m
        _ -> throwError' (BadArgument "length function can only be called with an iterable argument.")
    where
        toNum :: [a] -> Psnodig Value
        toNum = return . Number . toInteger . length

callFunction (FunctionCall "toString" args) = do
    when (length args /= 1)
        $ throwError' (WrongNumberOfArguments "Function 'toString' takes 1 argument: toString( value ).")
    str <- (evalExpr $ head args) >>= (flip stringifyValue) False
    return . Text $ str

callFunction (FunctionCall name args) = do
    maybeFunc <- lookupFunc name
    case maybeFunc of
        Just func -> do
            argsValues <- mapM evalExpr args
            applyFunction func argsValues
        Nothing -> throwError' (FunctionNotFound $ name ++ " function is not previously declared. Check the spelling.")

applyFunction :: FunctionDecl -> [Value] -> Psnodig Value
applyFunction (FunctionDecl name args stmts) values = do
    when (length args /= length values)
        $ throwError' (WrongNumberOfArguments $ "Function '" ++ name ++ "' takes a different number of args than provided!")
    pushFunctionScope
    zipWithM_ bindVar (map fstArg args) values
    res <- evalStmts stmts
    popFunctionScope
    either (const $ throwError' $ NoReturnError $ "Function '" ++ name ++ "' must return something!") return res


-- Main functions for execution

evalProgram :: Program -> Psnodig ()
evalProgram (Program _ structs funcs entryPoint) = do
    mapM_ processStructDecls structs
    mapM_ processFunDecl funcs
    case entryPoint of
        Just f -> void $ callFunction f
        Nothing -> throwError (MissingEntryPoint "No function call located to run your program.")
  where
    processFunDecl f@(FunctionDecl name _ _) = bindFunc name f

initialState :: ExecutionState
initialState = ExecutionState
    { structDecls = Map.empty
    , funcDecls = Map.empty
    , scopeStack = []
    , output = []
    }

runPsnodig :: Program -> IO (Either RuntimeError ExecutionState)
runPsnodig program =
    runExceptT $ execStateT (evalProgram program) initialState


-- Helpers

fstArg :: Argument -> String
fstArg (Argument a _) = a

bval :: Value -> Bool
bval Nil = False
bval (Boolean False) = False
bval (Number n) = if n > 0 then True else False
bval (Decimal d) = if d > 0.0 then True else False
bval (Text "") = False
bval (List []) = False
bval (HashSet hs) = not (Set.null hs)
bval (HashMap hm) = not (Map.null hm)
bval _ = True

toIterable :: Value -> Psnodig (Maybe [Value])
toIterable (Text t) = return . Just $ map (Text . return) t
toIterable (List exprs) = Just <$> mapM evalExpr exprs
toIterable (HashSet s) = Just <$> mapM evalExpr (Set.toList s)
toIterable (HashMap m) = Just <$> mapM (\(x, _) -> evalExpr x) (Map.toList m)
toIterable _ = return Nothing

fromNumber :: Value -> Maybe Integer
fromNumber (Number n) = Just n
fromNumber _ = Nothing

isNumber :: Value -> Bool
isNumber (Number _) = True
isNumber _ = False

fromDecimal :: Value -> Maybe Double
fromDecimal (Decimal d) = Just d
fromDecimal _ = Nothing

isDecimal :: Value -> Bool
isDecimal (Decimal _) = True
isDecimal _ = False

stringifyValue :: Value -> Bool -> Psnodig String
stringifyValue Nil _ = return "Nil"
stringifyValue (Boolean b) _ = return $ show b
stringifyValue (Number n) _ = return $ show n
stringifyValue (Decimal d) _ = return $ show d
stringifyValue (Text t) b = if b then return $ '\"' : t ++ "\"" else return t
stringifyValue (List l) _ = stringifyExprIterable "[" l "]"
stringifyValue (HashSet hs) _ = stringifyExprIterable "(" (Set.toList hs) ")"
stringifyValue (HashMap hm) _ = stringifyHMap $ Map.toList hm
stringifyValue (StructVal sf) _ = stringifyStruct sf

stringifyExprIterable :: String -> [Expression] -> String -> Psnodig String
stringifyExprIterable l list r = do
    vals <- mapM evalExpr list
    strList <- mapM (flip stringifyValue True) vals
    return $ l ++ intercalate ", " strList ++ r

stringifyHMap :: [(Expression, Expression)] -> Psnodig String
stringifyHMap exprs = do
    vals <- mapM evalPair exprs
    vals' <- mapM stringifyPair vals
    let strPairList = map (\(x, y) -> x ++ ": " ++ y) vals'
    return $ "{" ++ intercalate ", " strPairList ++ "}"
    where
        stringifyPair :: (Value, Value) -> Psnodig (String, String)
        stringifyPair (x, y) = do
            x' <- stringifyValue x False
            y' <- stringifyValue y False
            return (x', y')

stringifyStruct :: [(String, Value)] -> Psnodig String
stringifyStruct fields = do
    fields' <- mapM stringifyField fields
    return $ intercalate ", " fields'
    where
        stringifyField :: (String, Value) -> Psnodig String
        stringifyField (str, (StructVal innerFields)) = do
            stringFields <- stringifyStruct innerFields
            return $ str ++ ": (" ++ stringFields ++ ")"
        stringifyField (str, val) = do
            val' <- stringifyValue val False
            return $ str ++ ": " ++ val'

evalNestedIndex :: Value -> [Expression] -> Psnodig Value
evalNestedIndex val [] = return val
evalNestedIndex (List list) (indexExpr:indexExprs) = do
    maybeIndex <- evalExpr indexExpr
    case maybeIndex of
        Number n -> 
            if 0 <= n && n < fromIntegral (length list)
            then do
                nextVal <- evalExpr $ list !! fromInteger n
                evalNestedIndex nextVal indexExprs
            else throwError' (OutOfBounds $ "Attempted to access index " ++ show n ++ " of list with length " ++ show (length list) ++ ".")
        _ -> throwError' (BadArgument "Index must evaluate to a number.")
evalNestedIndex _ _ = throwError' (ListNotFound "Expected a list for index operation.")

replaceValueAtIndex :: [Expression] -> Int -> Value -> [Expression]
replaceValueAtIndex list index newVal =
    if length list == 1 then [(Constant newVal)]
    else case splitAt index list of
        (before, []) -> before ++ [Constant newVal]
        (before, _ : after) -> before ++ (Constant newVal) : after

updateValue :: Eq a => a -> b -> (a, b) -> (a, b)
updateValue targetKey newVal (key, val) =
    if key == targetKey then (key, newVal) else (key, val)

member :: String -> [(String, Value)] -> Bool
member x = any ((x ==) . fst)

throwError' :: RuntimeError -> Psnodig a
throwError' err = do
    currentOutput <- gets output
    throwError $ RuntimeErrorWithOutput currentOutput err
