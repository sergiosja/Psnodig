-- module Interpreter(f) where

-- f :: Int
-- f = 1

module Interpreter (
      ExecutionState(..)
      , RuntimeError(RuntimeErrorWithOutput)
      , runPsnodig
    ) where

import Syntax
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (void, liftM2)
import Data.List (find, intercalate)
import Data.Either (isRight)
import qualified Data.Map as Map
import qualified Data.Set as Set

type StructDecls = Map.Map String [String]
type FuncEnv = [(String, Function)] -- bruke map heller?
type Scope = [(String, Value)]
type ScopeStack = [[Scope]]

-- type Binding = (String, Value)
-- type BindingStack = [Binding]
-- type ScopeStack = [BindingStack]
-- type FunctionStack = [ScopeStack]

data ExecutionState = ExecutionState
    { structDecls :: StructDecls
    , funcEnv     :: FuncEnv
    , scopeStack  :: ScopeStack
    , output      :: [String]
} deriving (Show)

data RuntimeError =
      VariableNotFound String
    | FunctionNotFound String
    | StructNotFound String
    | ArithmeticError String
    | BadArgument String
    | WrongNumberOfArguments String
    | NoReturnError String
    | Error String
    | RuntimeErrorWithOutput [String] RuntimeError
    deriving (Eq, Show)

type Psnodig a = StateT ExecutionState (ExceptT RuntimeError IO) a


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
updateVar scopes@(scope : rest) var value =
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
        Just _ -> throwError' $ BadArgument $ "\"" ++ listName ++ "\" is not a list."
        Nothing -> throwError' $ VariableNotFound $ "No list \"" ++ listName ++ "\" previously defined."

updateNestedListVar :: Value -> [Expression] -> Value -> Psnodig Value
updateNestedListVar (List list) (indexExpr : []) value = do
    maybeIndex <- evalExpr indexExpr
    case maybeIndex of
        Number index ->
            if 0 <= index && (fromInteger index) < length list
            then return $ List $ replaceValueAtIndex list (fromInteger index) value
            else throwError' $ Error $ "Index " ++ show index ++ " out of bounds."
        _ -> throwError' $ BadArgument $ "List index must evaluate to a number."

updateNestedListVar (List list) (indexExpr : rest) value = do
    maybeIndex <- evalExpr indexExpr
    case maybeIndex of
        Number index ->
            if 0 <= index && (fromInteger index) < length list
            then do
                innerList <- evalExpr $ list !! (fromInteger index)
                (List list') <- updateNestedListVar innerList rest value
                return $ List $ replaceValueAtIndex list (fromInteger index) (List list')                
            else throwError' $ Error $ "Index " ++ show index ++ " out of bounds."
        _ -> throwError' $ BadArgument $ "List index must evaluate to a number."

updateNestedListVar _ _ _ = throwError' $ BadArgument "Sorry blud doesnt work 1"

findAndUpdateScope :: String -> Value -> Psnodig ()
findAndUpdateScope listName newList = do
    currScope@(ExecutionState { scopeStack = (scopes : rest) }) <- get
    case updateScopes scopes listName newList of
        Just newScopes -> put currScope { scopeStack = (newScopes : rest) }
        Nothing -> throwError' $ VariableNotFound $ "List " ++ listName ++ " not found."

updateScopes :: [[(String, Value)]] -> String -> Value -> Maybe [[(String, Value)]]
updateScopes [] _ _ = Nothing
updateScopes (scope:rest) listName newList =
    if member listName scope then
        Just $ (map (updateValue listName newList) scope) : rest
    else
        (scope :) <$> updateScopes rest listName newList

lookupFunc :: String -> Psnodig (Maybe Function)
lookupFunc func = do
    ExecutionState { funcEnv = env } <- get
    return $ lookup func env

bindFunc :: String -> Function -> Psnodig ()
bindFunc name func = do
    currScope@(ExecutionState { funcEnv = env }) <- get
    put currScope { funcEnv = (name, func):env }


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
                throwError' $ WrongNumberOfArguments $ "Provided " ++ show (length args) ++ " args to a struct that takes " ++ show (length args') ++ " args."
        Nothing -> throwError' $ StructNotFound $ "No struct \"" ++ maybeStruct ++ "\" previously defined."

updateStruct :: String -> [(String, Value)] -> Psnodig ()
updateStruct name fields =
    bindVar name (StructVal fields) -- fix this! not safe. should use something like [if k == name then (k, newVal) else (k, v) | k, v <- existingList]

updateStructField :: StructField -> Expression -> Psnodig ()
updateStructField (StructField structName fieldExpr) valueExpr = do
    case structName of
        VariableExp name -> do
            maybeStruct <- lookupVar name
            case maybeStruct of
                Just (StructVal fields) -> do
                    StructVal newEnv <- updateField fields fieldExpr valueExpr
                    updateStruct name newEnv
                _ -> throwError' $ BadArgument $ name ++ " is not a defined struct. 1"
        listIndex@(ListIndex listName indexExprs) -> do
            maybeStruct <- evalExpr listIndex
            case maybeStruct of
                StructVal fields -> do
                    StructVal newEnv <- updateField fields fieldExpr valueExpr
                    updateListVar listName indexExprs (StructVal newEnv)
                _ -> throwError' $ BadArgument $ listName ++ " does not contain a struct at given index."
        _ -> throwError' $ BadArgument "Cannot update struct field(s) of first argument."

updateField :: [(String, Value)] -> Expression -> Expression -> Psnodig Value
updateField env fieldExpr valueExpr = do
    case fieldExpr of
        VariableExp name -> do -- tree.right := valueExpr. "right" skal finnes i env
            newVal <- evalExpr valueExpr -- Verdi
            updateListEntry env name newVal
            -- return $ StructVal [(if name == arg then (arg, newVal) else (arg, val)) | (arg, val) <- env] -- hvis vi finner "right", erstatt dens verdi med newVal
        (ListIndex listName indexExprs) -> -- p.friends[0] := valueExpr.
            case lookup listName env of
                Just list@(List _) -> do
                    list' <- updateSingleFieldList list indexExprs valueExpr
                    return $ StructVal [(if listName == arg then (arg, list') else (arg, val)) | (arg, val) <- env]
                _ -> throwError' $ BadArgument "Blud this aint even a lizt!"
        (StructFieldExp (StructField field fieldExpr')) -> -- first.second.third := 5
            case field of
                VariableExp name -> do
                    case lookup name env of -- ("second", [(), .., ()]) skal finnes i env. kjør rekursivt med de fieldsene
                        Just (StructVal fields) -> do
                            fields' <- updateField fields fieldExpr' valueExpr
                            updateListEntry env name fields'
                            -- return $ StructVal [(if name == arg then (arg, fields') else (arg, val)) | (arg, val) <- env]
                        _ -> throwError' $ BadArgument "Line 210 in Interpreter.hs! This aint fields! Maybe I should draw this or summin"
                (ListIndex listName indexExprs) -> do -- first.second[0].third := 4
                    case lookup listName env of
                        Just list@(List _) -> do
                            list' <- updateRecursiveFieldList list indexExprs fieldExpr' valueExpr
                            updateListEntry env listName list'
                        _ -> throwError' $ BadArgument "Sorry blud doesnt work 2"
                _ -> throwError' $ BadArgument "Sorry blud doesnt work 3"
                        
        _ -> throwError' $ BadArgument "Sorry blud doesnt work 4"


updateSingleFieldList :: Value -> [Expression] -> Expression -> Psnodig Value
updateSingleFieldList (List list) (indexExpr : []) valueExpr = do
    maybeIndex <- evalExpr indexExpr
    value <- evalExpr valueExpr
    case maybeIndex of
        Number index ->
            if 0 <= index && (fromInteger index) < length list
            then return $ List (replaceValueAtIndex list (fromInteger index) value)
            else throwError' $ BadArgument "List index outtta boundz!"
        _ -> throwError' $ BadArgument "Bruh!"

updateSingleFieldList (List list) (indexExpr : rest) valueExpr = do
    maybeIndex <- evalExpr indexExpr
    case maybeIndex of
        Number index ->
            if 0 <= index && (fromInteger index) < length list
            then do
                let listExpr = list !! (fromInteger index) -- tror dette vil være en expression av typen (Constant (List [..]))
                list' <- evalExpr listExpr
                list'' <- updateSingleFieldList list' rest valueExpr
                return $ List (replaceValueAtIndex list (fromInteger index) list'')
            else throwError' $ BadArgument "List index outtta boundz!"
        _ -> throwError' $ BadArgument "Not int!"

updateSingleFieldList _ _ _ = throwError' $ BadArgument "Sorry blud doesnt work 5"


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
            else throwError' $ BadArgument "List index outtta boundz!"
        _ -> throwError' $ BadArgument "Sorry blud doesnt work 6"

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
            else throwError' $ BadArgument "List index outtta boundz!"
        _ -> throwError' $ BadArgument "Not int!"

updateRecursiveFieldList _ _ _ _ = throwError' $ BadArgument "Sorry blud doesnt work 7"


updateListEntry :: [(String, Value)] -> String -> Value -> Psnodig Value
updateListEntry env name newVal = return $ StructVal [(if name == arg then (arg, newVal) else (arg, val)) | (arg, val) <- env]

-- Expressions

evalExpr :: Expression -> Psnodig Value
evalExpr (Constant v) = return v
evalExpr (VariableExp var) = do
    maybeVar <- lookupVar var
    case maybeVar of
        Just val -> return val
        Nothing -> throwError' $ VariableNotFound $ "Variable " ++ var ++ " not found!"
evalExpr (BinaryExp op expr1 expr2) = do
    res <- operate op expr1 expr2
    case res of
        Left errMsg -> throwError' $ ArithmeticError errMsg
        Right val -> return val
evalExpr (ListIndex var indexExprs) = do
    maybeList <- lookupVar var
    case maybeList of
        Just val -> evalNestedIndex val indexExprs
        Nothing  -> throwError' $ VariableNotFound $ "List" ++ var ++ "not found!"
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
            else throwError' $ BadArgument $ "Incorrect number of arguments provided for struct " ++ maybeStruct ++ "."
        Nothing -> throwError' $ BadArgument $ "Struct " ++ maybeStruct ++ " is not defined."

evalStructFieldExprMain :: StructField -> Psnodig Value
evalStructFieldExprMain (StructField structName fieldExpr) =
    case structName of
        VariableExp name -> do
            maybeStruct <- lookupVar name
            case maybeStruct of
                Just (StructVal fields) -> evalStructFieldExprRec fields fieldExpr
                invalidField -> throwError' $ BadArgument $ "Either '" ++ name ++ "' is not a defined struct, or a field of '" ++
                                                           (show invalidField) ++ "' is attempted evaluated. Make sure to check your base cases!"
        listIndex@(ListIndex listName _) -> do
            maybeStruct <- evalExpr listIndex
            case maybeStruct of
                StructVal fields -> evalStructFieldExprRec fields fieldExpr
                _ -> throwError' $ BadArgument $ listName ++ " does not contain a struct at given index."
        _ -> throwError' $ BadArgument "Cannot lookup struct field(s) of first argument."

evalStructFieldExprRec :: [(String, Value)] -> Expression -> Psnodig Value
evalStructFieldExprRec fields expr = do
    case expr of
        VariableExp name ->
            case lookup name fields of
                Just v -> return v
                Nothing -> throwError' $ BadArgument "Invalid struct field."
        (ListIndex name indexExprs) ->
            case lookup name fields of
                Just list@(List _) -> evalNestedIndex list indexExprs -- dobbelsjekk denne
                _ -> throwError' $ BadArgument "Invalid struct field."
        (StructFieldExp (StructField field expr')) ->
            case field of
                VariableExp name -> do
                    case lookup name fields of
                        Just (StructVal fields') -> evalStructFieldExprRec fields' expr'
                        _ -> throwError' $ BadArgument $ "Tried calling " ++ name ++ " with invalid field."
                (ListIndex name indexExprs) ->
                    case lookup name fields of
                        Just list@(List _) -> do
                            StructVal fields' <- evalNestedIndex list indexExprs
                            evalStructFieldExprRec fields' expr'
                        _ -> throwError' $ BadArgument "Invalid struct field."
                _ -> throwError' $ BadArgument "Tried to access field of non-struct identifier. 1"
        _ -> throwError' $ BadArgument "Tried to access field of non-struct identifier. 2"


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
operate' Plus (Text s1) (Text s2) = Right $ Text $ s1 ++ s2
operate' Minus (Number x) (Number y) = Right $ Number $ x - y
operate' Times (Number x) (Number y) = Right $ Number $ x * y
operate' Division (Number _) (Number 0) = Left "Division by zero!"
-- should probs fix floats/doubles too
operate' Division (Number x) (Number y) = Right $ Number $ div x y
operate' Modulo (Number _) (Number 0) = Left "Modulo by zero!"
operate' Modulo (Number x) (Number y) = Right $ Number $ mod x y

operate' LessThan (Number x) (Number y) = Right $ Boolean $ x < y
operate' LessThanEqual (Number x) (Number y) = Right $ Boolean $ x <= y
operate' GreaterThan (Number x) (Number y) = Right $ Boolean $ x > y
operate' GreaterThanEqual (Number x) (Number y) = Right $ Boolean $ x >= y

operate' Equal (Number x) (Number y) = Right $ Boolean $ x == y
operate' Equal Nil Nil = Right $ Boolean True
operate' Equal (Text t1) (Text t2) = Right $ Boolean $ t1 == t2
operate' Equal _ _ = Right $ Boolean False

operate' NotEqual (Number x) (Number y) = Right $ Boolean $ x /= y
operate' NotEqual Nil Nil = Right $ Boolean False
operate' NotEqual (Text t1) (Text t2) = Right $ Boolean $ t1 /= t2
operate' NotEqual _ _ = Right $ Boolean False

-- operate' And x y = return $ Boolean $ (bval x) && (bval y)
-- operate' Or x y = return $ Boolean $ (bval x) || (bval y)

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
evalStmt (Assignment assTarget assValue) = do -- nødvendig med do her?
    case assValue of
        (ExpressionValue expr) -> do
            value <- evalExpr expr
            case assTarget of
                (VariableTarget var) ->
                    bindVar var value >> return (Left ())
                (ListIndexTarget var indexExprs) ->
                    updateListVar var indexExprs value >> return (Left ())
                (StructFieldTarget structField) ->
                    updateStructField structField expr >> return (Left ())
        (StructValue struct) ->
            case assTarget of
                (VariableTarget var) ->
                    bindStruct var struct >> return (Left ())
                (ListIndexTarget var indexExprs) -> do
                    fields <- evalExpr (StructExpr struct)
                    updateListVar var indexExprs fields >> return (Left ())
                (StructFieldTarget structField) ->
                    updateStructField structField (StructExpr struct) >> return (Left ())

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
    maybeIterable <- toIterable expr
    case maybeIterable of
        Just values -> foldM (executeForEachLoopScope ident stmts) (Left ()) values
        Nothing -> throwError' $ BadArgument "Collection argument is not an iterable. If this is a function call, dereference it before applying as a collection."

evalStmt (For ident expr1 expr2 stmts) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case (fromNumber val1, fromNumber val2) of
        (Just n1, Just n2) ->
            foldM (executeForLoopScope ident stmts) (Left ()) (forRange n1 n2)
        _ -> throwError' $ BadArgument "Range must be whole numbers!"

evalStmt (CallStmt f) =
    callFunction f >> return (Left ())
evalStmt (Return expr) =
    Right <$> evalExpr expr
evalStmt (HashStmt stmt) =
    evalStmt stmt
evalStmt (AnnotationStmt _ stmts) =
    evalStmts stmts

-- fix these ?
evalStmt (Break) = undefined
evalStmt (Continue) = undefined


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

-- må legge til feks add to hashmap, add to set osv.
-- min, max
callFunction :: FunctionCall -> Psnodig Value
callFunction (FunctionCall "floor" args) = do
    when (length args /= 1)
        $ throwError' $ WrongNumberOfArguments "Function 'floor' takes 1 argument: floor( number )."
    value <- evalExpr $ head args
    case value of
        Number n -> return $ value -- fix this obvs
        _ -> throwError' $ BadArgument "Function 'floor' can only be applied on integers."

callFunction (FunctionCall "ceil" args) = do
    when (length args /= 1)
        $ throwError' $ WrongNumberOfArguments "Function 'ceil' takes 1 argument: ceil( number )."
    value <- evalExpr $ head args
    case value of
        Number n -> return $ value -- fix this obvs
        _ -> throwError' $ BadArgument "Function 'ceil' can only be applied on integers."

callFunction (FunctionCall "min" args) = do
    values <- mapM evalExpr args
    if any (not . isNumber) values
    then throwError' $ BadArgument "All arguments to function 'min' must be integers."
    else return . Number . minimum . map (\(Number x) -> x) $ values

callFunction (FunctionCall "max" args) = do
    values <- mapM evalExpr args
    if any (not . isNumber) values
    then throwError' $ BadArgument "All arguments to function 'max' must be integers."
    else return . Number . maximum . map (\(Number x) -> x) $ values

callFunction (FunctionCall "get" args) = do
    when (length args /= 2)
        $ throwError' $ WrongNumberOfArguments "Function 'get' takes 2 arguments: get( value , map )."
    let mapExpr = head $ tail args
    case mapExpr of
        VariableExp mapName -> do
            maybeMap <- evalExpr mapExpr
            case maybeMap of
                HashMap m -> do
                    case Map.lookup (head args) m of
                        Just v -> evalExpr v
                        Nothing -> throwError' $ BadArgument $ "Function 'get' takes two arguments: get( key, map ). " ++ (show $ head args) ++ " is likely an invalid key."
                _ -> throwError' $ BadArgument $ "Function 'get' takes two arguments: get( key, map ). " ++ mapName ++ " is likely an invalid Hashmap."
        _ -> throwError' $ BadArgument "Function 'get' takes two arguments: get( key, map )."

callFunction (FunctionCall "append" args) = do
    when (length args /= 2)
        $ throwError' $ WrongNumberOfArguments "Function 'append' takes 2 arguments: append( value , list )."
    let listExpr = args !! 1
    maybeList <- evalExpr listExpr
    case maybeList of
        List l -> do
            case listExpr of
                VariableExp listName -> do
                    val <- evalExpr $ head args
                    let newList = List $ l ++ [(Constant val)]
                    findAndUpdateScope listName newList
                    return $ Number 1
                -- ListIndex listName indexes -> do
                --     val <- evalExpr $ head args
                    -- vi har en List l, som er en liste med lister
                    -- listName er faktiske lista som ligger i skopet.
                    -- indexes er lista som skal appendes til

                    -- altså: ta lista som ligger på indexes av listName,
                    -- og legg til `val` der

                    -- altså:
                    -- legg denne nye lista inn på stedet der `listName indexes` lå før
                    -- ny variabel som har disse oppdaterte listene
                    -- kjør findAndUpdateScope listName newList


                    -- findAndUpdateScope listName newList
                    -- return $ Number 1

                _ -> throwError' $ BadArgument "Function 'append' takes two arguments: append( value , list ). The second argument is likely an invalid list."
        _ -> throwError' $ BadArgument $ "Function 'append' yields error. Either '" ++ (show $ head args) -- ++ "' is an invalid value, or '" ++ listName ++ "' is an invalid list."



-- callFunction (FunctionCall "add" args) = do -- sets and hashmaps

callFunction (FunctionCall "print" args) = do
    values <- mapM evalExpr args
    strings <- mapM (\v -> stringifyValue v False) values
    modify (\s -> s { output = output s ++ strings })
    return . Number . toInteger . length $ args

callFunction (FunctionCall "length" args) = do
    maybeIterable <- evalExpr $ head args
    case maybeIterable of
        (Text t) -> toNum t
        (List l) -> toNum l
        (HashSet s) -> toNum $ Set.toList s
        (HashMap m) -> toNum $ Map.toList m
        _ -> throwError' $ BadArgument "length can only be called with iterable!"
    where
        toNum :: [a] -> Psnodig Value
        toNum = return . Number . toInteger . length

callFunction (FunctionCall name args) = do
    maybeFunc <- lookupFunc name
    case maybeFunc of
        Just func -> do
            argsValues <- mapM evalExpr args
            applyFunction func argsValues
        Nothing -> throwError' $ FunctionNotFound name

applyFunction :: Function -> [Value] -> Psnodig Value
applyFunction (Function name args stmts) values = do
    when (length args /= length values)
        $ throwError' $ WrongNumberOfArguments $ "Function '" ++ name ++ "' takes a different number of args than provided!"
    pushFunctionScope
    zipWithM_ bindVar (map fstArg args) values
    res <- evalStmts stmts
    popFunctionScope
    either (const $ throwError' $ NoReturnError $ "Function '" ++ name ++ "' must return something!") return res


-- Main functions for execution

evalProgram :: Program -> Psnodig ()
evalProgram (Program structs funcs entryPoint) = do
    mapM_ processStructDecls structs
    mapM_ processFunDecl funcs
    case entryPoint of
        Just f -> void $ callFunction f
        Nothing -> throwError $ BadArgument "No function call to run the program."
  where
    processFunDecl f@(Function name _ _) = bindFunc name f

initialState :: ExecutionState
initialState = ExecutionState
    { structDecls = Map.empty
    , funcEnv = []
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
bval (Text "") = False
bval (List []) = False
bval _ = True

toIterable :: Expression -> Psnodig (Maybe [Value])
toIterable (Constant v) = toIterableValue v
toIterable (VariableExp var) = do
    maybeVar <- lookupVar var
    case maybeVar of
        Just v -> toIterableValue v
        Nothing -> return Nothing
toIterable e@(ListIndex _ _) = do
    v <- evalExpr e
    toIterableValue v
toIterable e@(StructFieldExp _) = do
    v <- evalExpr e
    toIterableValue v
toIterable _ = return Nothing

toIterableValue :: Value -> Psnodig (Maybe [Value])
toIterableValue (Text t) = return . Just $ map (Text . return) t
toIterableValue (List exprs) = Just <$> mapM evalExpr exprs
toIterableValue (HashSet s) = Just <$> mapM evalExpr (Set.toList s)
toIterableValue (HashMap m) = Just <$> mapM (\(x, _) -> evalExpr x) (Map.toList m) -- can access values by calling the map with keys!
                -- (HashMap m) -> Just <$> mapM (\(x, y) -> liftM2 (,) (evalExpr x) (evalExpr y)) (Map.toList m)
toIterableValue _ = return Nothing

fromNumber :: Value -> Maybe Integer
fromNumber (Number n) = Just n
fromNumber _ = Nothing

isNumber :: Value -> Bool
isNumber (Number _) = True
isNumber _ = False

-- deref :: Value -> [Expression]
-- deref (List xs) = xs

stringifyValue :: Value -> Bool -> Psnodig String
stringifyValue v fromList = case v of
    Text t -> if fromList then return $ '\"' : t ++ "\"" else return t
    Nil -> return "\"Nil\""
    Number n ->  return $ show n
    Boolean b -> return $ show b
    HashSet s -> return $ show s
    HashMap m -> return $ show m
    List l -> stringifyList l
    StructVal fields -> return $ show fields -- return $ stringifyStruct fields

-- stringifyStruct :: [(String, Value)] -> String
-- stringifyStruct fields =
    -- intercalate ", " $ map (\(k, v) -> k ++ " -> " ++ show v) fields

stringifyList :: [Expression] -> Psnodig String
stringifyList list = do
    vals <- mapM evalExpr list
    strList <- mapM (flip stringifyValue True) vals
    return $ "[" ++ intercalate ", " strList ++ "]"

evalNestedIndex :: Value -> [Expression] -> Psnodig Value
evalNestedIndex val [] = return val
evalNestedIndex (List list) (indexExpr:indexExprs) = do
    maybeIndex <- evalExpr indexExpr
    case maybeIndex of
        Number n -> 
            if 0 <= n && n < fromIntegral (length list)
            then do
                nextVal <- evalExpr $ list !! fromInteger n -- burde sjekke at list !! fromInteger n er lov! at det ikke gir outofbounds ellerno
                evalNestedIndex nextVal indexExprs
            else throwError' $ BadArgument $ "Attempted to access index " ++ (show n) ++ " of list with length " ++ (show $ length list) ++ "!"
        _ -> throwError' $ BadArgument "Index must evaluate to a number"
evalNestedIndex _ _ = throwError' $ BadArgument "Expected a list for index operation"

-- denne er avhengig av at man VET at index er good fra før!
replaceValueAtIndex :: [Expression] -> Int -> Value -> [Expression]
replaceValueAtIndex list index newVal =
    if length list == 1 then [(Constant newVal)]
    -- if length list <= index then [(Constant newVal)]
    else let (before, _ : after) = splitAt index list
    in before ++ (Constant newVal) : after

updateValue :: Eq a => a -> b -> (a, b) -> (a, b)
updateValue targetKey newVal (key, val) =
    if key == targetKey then (key, newVal) else (key, val)

member :: String -> [(String, Value)] -> Bool
member x = any ((x ==) . fst)

throwError' :: RuntimeError -> Psnodig a
throwError' err = do
    currentOutput <- gets output
    throwError $ RuntimeErrorWithOutput currentOutput err

-- -- Thought:
-- -- There are no global variables
-- -- Funcs and structs are global, but variables are always local to their own functions

{-
func f() {
    x = 5
    return g()
}

func g() {
    x = 10 -- is the original x overwritten?
    return x
}

perhaps we should have a stack of variables
all new, local variables are put on top of the stack.
when we use a variable, we look from the top and down, until we find a matching one.

e.g. [x=5, x=10]
when using x, the first one we see is the =10 one

and then, when leaving a function, we empty the stack.
so we keep a counter as well, on how many elements we've added to the stack
a global counter, I guess?
at the stard and at the end of the program, it should be 0

what about this

struct P {
    age int,
    friends int
}

func m() {
    prsn := struct P(29, 12)
    f(prsn)
}

yh okey, only look in struct scope when declaring new structs, but not when looking for variables :)

-}


-- {- problemet her er at bind egt skal funke for ting som
--     x = 5 og h = Height(1, 2, 3)

--     når vi har bare struct Height { a int b int c int } etc., så har vi
--     ikke egentlig noen 
--  -}
