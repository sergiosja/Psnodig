-- module Interpreter(f) where

-- f :: Int
-- f = 1

module Interpreter (ExecutionState(..), runPsnodig) where

import Syntax
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (void, liftM2)
import Data.List (find, intercalate)
import Data.Either (isRight)
import qualified Data.Map as Map
import qualified Data.Set as Set

type StructDecls = Map.Map String [String]
type StructEnv = Map.Map String [(String, Value)]
type FuncEnv = [(String, Function)] -- bruke map heller?
type Scope = [(String, Value)] -- bruke map heller?
type ScopeStack = [Scope]

data ExecutionState = ExecutionState {
    structDecls :: StructDecls,
    structEnv :: StructEnv,
    funcEnv :: FuncEnv,
    scopeStack :: ScopeStack,
    output :: [String]
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
    deriving (Eq, Show)

type Psnodig a = StateT ExecutionState (ExceptT RuntimeError IO) a


-- Scoping and binding

pushScope :: Psnodig ()
pushScope = do
    currScope@(ExecutionState { scopeStack = scopes }) <- get
    put currScope { scopeStack = ([] : scopes) }

popScope :: Psnodig ()
popScope = do
    currScope@(ExecutionState { scopeStack = (_ : scopes) }) <- get
    put currScope { scopeStack = scopes }

lookupVar :: String -> Psnodig (Maybe Value)
lookupVar var = do
    ExecutionState { scopeStack = scopes } <- get
    return $ searchScopes scopes var
    where
        searchScopes [] _ = Nothing
        searchScopes (scope : rest) variable =
            case lookup variable scope of
                Just v -> Just v
                Nothing -> searchScopes rest variable

bindVar :: String -> Value -> Psnodig ()
bindVar var value = do
    currScope@(ExecutionState { scopeStack = (top:rest) }) <- get
    put currScope { scopeStack = ((var, value):top) : rest }

-- Will likely have to fix something with LocalStructField for sets and maps!
updateListVar :: String -> Expression -> Value -> Psnodig ()
updateListVar listName indexExpr value = do
    maybeList <- lookupVar listName
    case maybeList of
        Just (List list) -> do
            indexValue <- evalExpr indexExpr
            case indexValue of
                (Number index) ->
                    case 0 <= index && (fromInteger index) < length list of
                        True ->
                            let updatedList = replaceValueAtIndex list (fromInteger index) value -- [(if n == index then (n, newVal) else (n,v)) | (n, v) <- fields] ???
                            in findAndUpdateScope listName (List updatedList)
                        False -> throwError $ Error $ "Index " ++ show index ++ " out of bounds of " ++ listName ++ "."
                _ -> throwError $ BadArgument $ "List index must be a number."
        Just _ -> throwError $ BadArgument $ "\"" ++ listName ++ "\" is not a list."
        Nothing -> throwError $ VariableNotFound $ "No list \"" ++ listName ++ "\" previously defined."

findAndUpdateScope :: String -> Value -> Psnodig ()
findAndUpdateScope listName newList = do
    currScope@(ExecutionState { scopeStack = scopes }) <- get
    case updateScopes scopes listName newList of
        Just newScopes -> put currScope { scopeStack = newScopes }
        Nothing -> throwError $ VariableNotFound $ "List " ++ listName ++ " not found." -- should not be possible due to the check in updateListVar

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

lookupStruct :: String -> Psnodig (Maybe Value)
lookupStruct var = do
    ExecutionState { structEnv = env } <- get
    case Map.lookup var env of
        Just fields -> return $ Just (StructVal fields)
        -- Just v -> return $ Just (StructVal (Struct var (map (\p -> (Constant (snd p))) v)))
        Nothing -> return $ Nothing

bindStruct :: String -> Struct -> Psnodig ()
bindStruct name (Struct maybeStruct args) = do
    scope@(ExecutionState { structDecls = decls, structEnv = env }) <- get
    case Map.lookup maybeStruct decls of
        Just args' -> do
            if length args == length args' then do
                values <- mapM evalExpr args
                let newStructEnv = Map.insert name (zip args' values) env -- se := struct P..    "se" -> [("age", 24), ("friends", 1)]
                put scope { structEnv = newStructEnv }
            else
                throwError $ WrongNumberOfArguments $ "Provided " ++ show (length args) ++ " args to a struct that takes " ++ show (length args') ++ " args."
        Nothing -> throwError $ StructNotFound $ "No struct \"" ++ maybeStruct ++ "\" previously defined."


 -- skal bare kalles fra updateStructField, så skal være safe
updateStruct :: String -> [(String, Value)] -> Psnodig ()
updateStruct name fields = do
    scope@(ExecutionState { structEnv = env }) <- get
    case Map.lookup name env of
        Just _ -> do
            let env' = Map.insert name fields env
            put scope { structEnv = env' }
        Nothing -> throwError $ StructNotFound $ "No struct \"" ++ name ++ "\" previously defined."


    -- | VariableExp String
    -- | ListIndex String Expression -- skulle fikse denne også til | [Expression]
    -- | StructFieldExp StructField

-- tree.left := 5           (StructField (VariableExp "tree") (VariableExp "left"))
-- arr[5].left := 5         (StructField (ListIndex "arr" (Constant (Number 5))) (VariableExp "left"))
-- tree.right.left := 5     (StructField (VariableExp "tree") (StructFieldExp (StructField (VariableExp "right") (VariableExp "leff"))))



updateStructField :: StructField -> Expression -> Psnodig ()
updateStructField (StructField structName fieldExpr) valueExpr = do
    case structName of
        VariableExp name -> do
            maybeStruct <- lookupStruct name
            case maybeStruct of
                Just (StructVal fields) -> do
                    StructVal newEnv <- updateField fields fieldExpr valueExpr
                    updateStruct name newEnv
                Nothing -> throwError $ BadArgument $ name ++ " is not a defined struct."
        listIndex@(ListIndex listName indexExpr) -> do
            maybeStruct <- evalExpr listIndex
            case maybeStruct of
                StructVal fields -> do
                    StructVal newEnv <- updateField fields fieldExpr valueExpr
                    updateListVar listName indexExpr (StructVal newEnv)
                _ -> throwError $ BadArgument $ listName ++ " does not contain a struct at given index."
        _ -> throwError $ BadArgument "Cannot update struct field(s) of first argument."


updateField :: [(String, Value)] -> Expression -> Expression -> Psnodig Value
updateField env fieldExpr valueExpr = do
    case fieldExpr of
        VariableExp name -> do -- tree.right := valueExpr. "right" skal finnes i env
            newVal <- evalExpr valueExpr -- Verdi
            return $ StructVal [(if name == arg then (arg, newVal) else (arg, val)) | (arg, val) <- env] -- hvis vi finner "right", erstatt dens verdi med newVal
        (ListIndex listName indexExpr) -> do -- p.friends[0] := valueExpr.
            newVal <- evalExpr valueExpr -- Verdi
            maybeIndex <- evalExpr indexExpr
            case maybeIndex of
                (Number n) -> if 0 <= n && (fromInteger n) < length env
                              then return $ StructVal [(if listName == arg then (arg, List (replaceValueAtIndex (deref val) (fromInteger n) newVal)) else (arg, val)) | (arg, val) <- env]
                              -- ("friends", List [expr]) skal finnes i env. kjør replaceValueAtIndex med indeksen og verdien
                              else throwError $ BadArgument "List index out of range! updateFielddldldl"
                _ -> throwError $ BadArgument "Must provide index!! > updateField"
        (StructFieldExp (StructField field fieldExpr')) -> -- first.second.third := 5
            case field of
                VariableExp name -> do
                    case lookup name env of -- ("second", [(), .., ()]) skal finnes i env. kjør rekursivt med de fieldsene
                        Just (StructVal fields) -> do
                            fields' <- updateField fields fieldExpr' valueExpr
                            return $ StructVal [(if name == arg then (arg, fields') else (arg, val)) | (arg, val) <- env]
                        Nothing -> throwError $ BadArgument "Line 210 in Interpreter.hs! This aint fields! Maybe I should draw this or summin"
                (ListIndex listName indexExpr) -> do -- first.second[0].third := 4
                    case lookup listName env of
                        Just (List list) -> do -- env = [('noe', noe), ('annet', annet), ("second", [1, 2, .., n])]
                            maybeIndex <- evalExpr indexExpr
                            case maybeIndex of
                                Number n -> if 0 <= n && (fromInteger n) < length list
                                            then do
                                                StructVal fields <- evalExpr $ list !! (fromInteger n)
                                                updatedList <- updateField fields fieldExpr' valueExpr
                                                let newList = List (replaceValueAtIndex list (fromInteger n) updatedList)
                                                return $ StructVal [(if listName == arg then (arg, newList) else (arg, val)) | (arg, val) <- env]
                                            else throwError $ BadArgument "blah blah"
                                _ -> throwError $ BadArgument "Sorry baby this is not a valid index"
                        Nothing -> throwError $ BadArgument "something something updateField"


-- updateStructField :: StructField -> Expression -> Psnodig ()
-- updateStructField (StructField expr1 expr2) valueExpr = do
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


-- updateStructField (StructField struct field) expr = do
--     scope@(ExecutionState { structEnv = env }) <- get
--     case Map.lookup struct env of
--         Just args -> case lookup field args of
--             Just _ -> do
--                 value <- evalExpr expr
--                 let args' = map (\(x, y) -> if x == field then (x, value) else (x, y)) args
--                 let env' = Map.insert struct args' env
--                 put scope { structEnv = env' }
--             Nothing -> throwError $ BadArgument $ "No field \"" ++ field ++ "\" on struct " ++ struct
--         Nothing -> throwError $ StructNotFound $ "No struct \"" ++ struct ++ "\" previously defined."

lookupStructField :: ExecutionState -> String -> String -> Maybe Value
lookupStructField scope name field = do
    case Map.lookup name (structEnv scope) of
        Just fields -> lookup field fields
        Nothing -> Nothing


-- Expressions

evalExpr :: Expression -> Psnodig Value
evalExpr (Constant v) = return v
evalExpr (VariableExp var) = do
    maybeVar <- lookupVar var
    case maybeVar of
        Just val -> return val
        Nothing -> do
            maybeStruct <- lookupStruct var
            case maybeStruct of
                Just val -> return val
                Nothing -> throwError $ VariableNotFound $ "Variable " ++ var ++ " not found!"
evalExpr (BinaryExp op expr1 expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case operate op val1 val2 of
        Left errMsg -> throwError $ ArithmeticError errMsg
        Right val -> return val
evalExpr (ListIndex var indexExprs) = do
    maybeList <- lookupVar var
    case maybeList of
        Just val -> evalNestedIndex val indexExprs
        Nothing  -> throwError $ VariableNotFound "Variable not found!"
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
            else throwError $ BadArgument $ "Incorrect number of arguments provided for struct " ++ maybeStruct ++ "."
        Nothing -> throwError $ BadArgument $ "Struct " ++ maybeStruct ++ " is not defined."

evalStructFieldExprMain :: StructField -> Psnodig Value
evalStructFieldExprMain (StructField structName fieldExpr) =
    case structName of
        VariableExp name -> do
            maybeStruct <- lookupStruct name
            case maybeStruct of
                Just (StructVal fields) -> evalStructFieldExprRec fields fieldExpr
                Nothing -> throwError $ BadArgument $ name ++ " is not a defined struct."
        listIndex@(ListIndex listName _) -> do
            maybeStruct <- evalExpr listIndex
            case maybeStruct of
                StructVal fields -> evalStructFieldExprRec fields fieldExpr
                _ -> throwError $ BadArgument $ listName ++ " does not contain a struct at given index."
        _ -> throwError $ BadArgument "Cannot lookup struct field(s) of first argument."

evalStructFieldExprRec :: [(String, Value)] -> Expression -> Psnodig Value
evalStructFieldExprRec fields expr = do
    case expr of
        VariableExp name ->
            case lookup name fields of
                Just v -> return v
                Nothing -> throwError $ BadArgument "Invalid struct field."
        (ListIndex name indexExp) ->
            case lookup name fields of
                Just (List list) -> do
                    maybeIndex <- evalExpr indexExp
                    case maybeIndex of
                        Number index ->
                            if 0 <= index && (fromInteger index) < length list
                            then evalExpr $ list !! (fromInteger index) -- dobbelsjekk denne
                            else throwError $ Error $ "Index " ++ show index ++ " out of bounds of."
                        _ -> throwError $ BadArgument "Not a numberrr!!"
                Nothing -> throwError $ BadArgument "Invalid struct field."
        (StructFieldExp (StructField field expr')) ->
            case field of
                VariableExp name -> do
                    case lookup name fields of
                        Just (StructVal fields') -> evalStructFieldExprRec fields' expr'
                        Nothing -> throwError $ BadArgument $ "Tried calling " ++ name ++ " with invalid field."
                (ListIndex name indexExp) ->
                    case lookup name fields of
                        Just (List list) -> do
                            maybeIndex <- evalExpr indexExp
                            case maybeIndex of
                                Number index ->
                                    if 0 <= index && (fromInteger index) < length list
                                    then do
                                        StructVal fields' <- evalExpr $ list !! (fromInteger index)
                                        evalStructFieldExprRec fields' expr'
                                    else throwError $ BadArgument $ "Tried calling " ++ name ++ " with invalid field."
                                _ -> throwError $ BadArgument "not numberrr!"
                        Nothing -> throwError $ BadArgument "Invalid struct field."
                _ -> throwError $ BadArgument "Tried to access field of non-struct identifier."
        _ -> throwError $ BadArgument "Tried to access field of non-struct identifier."

-- Operators

operate :: Operator -> Value -> Value -> Either String Value
operate Plus (Number x) (Number y) = return $ Number $ x + y
operate Plus (Text s1) (Text s2) = return $ Text $ s1 ++ s2
operate Minus (Number x) (Number y) = return $ Number $ x - y
operate Times (Number x) (Number y) = return $ Number $ x * y
operate Division (Number _) (Number 0) = Left "Division by zero!"
-- should probs fix floats/doubles too
operate Division (Number x) (Number y) = return $ Number $ div x y
operate Modulo (Number _) (Number 0) = Left "Modulo by zero!"
operate Modulo (Number x) (Number y) = return $ Number $ mod x y

operate LessThan (Number x) (Number y) = return $ Boolean $ x < y
operate LessThanEqual (Number x) (Number y) = return $ Boolean $ x <= y
operate GreaterThan (Number x) (Number y) = return $ Boolean $ x > y
operate GreaterThanEqual (Number x) (Number y) = return $ Boolean $ x >= y
operate Equal (Number x) (Number y) = return $ Boolean $ x == y
operate NotEqual (Number x) (Number y) = return $ Boolean $ x /= y
operate And x y = return $ Boolean $ (bval x) && (bval y)
operate Or x y = return $ Boolean $ (bval x) || (bval y)

operate _ _ _ = Left "Incompatible operands!"


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
        (ExpressionValue expr) -> do -- ... := expression
            value <- evalExpr expr
            case assTarget of
                (VariableTarget var) -> -- k := 5
                    bindVar var value >> return (Left ())
                (ListIndexTarget var indexExpr) -> -- k[1] := [struct P(1, 2, 3)], her lagres vel (List [Constant (StructVal [(1, ),(2, .),3]), ..])
                    updateListVar var indexExpr value >> return (Left ())
                (StructFieldTarget structField) -> -- a.b.c := 1+1
                    updateStructField structField expr >> return (Left ())
        (StructValue struct) -> -- ... := struct P(..)
            case assTarget of
                (VariableTarget var) -> -- k := struct P(1, 2, 3)
                    bindStruct var struct >> return (Left ()) -- lagrer k -> [(,), (,), (,)] i structEnv
                (ListIndexTarget var expr) -> do -- k[0] := struct P(1, 2, 3)
                    fields <- evalExpr (StructExpr struct)
                    updateListVar var expr fields >> return (Left ())
                (StructFieldTarget structField) -> -- a.b.c := struct P(1, 2, 3)
                    updateStructField structField (StructExpr struct) >> return (Left ()) -- sjekk om jeg egt er keen på det her eller heller vil ha [(a, b)]

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
        Nothing -> throwError $ BadArgument "Collection argument is not an iterable. If this is a function call, dereference it before applying as a collection."

evalStmt (For ident expr1 expr2 stmts) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case (fromNumber val1, fromNumber val2) of
        (Just n1, Just n2) -> foldM (executeForLoopScope ident stmts) (Left ()) [n1 .. n2]
        _ -> throwError $ BadArgument "Range must be whole numbers!"

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


-- Functions

-- må legge til feks add to hashmap, add to set osv.
-- length og sånt, ceil, floor osv.
callFunction :: FunctionCall -> Psnodig Value
callFunction (FunctionCall "print" args) = do
    value <- evalExpr $ head args -- burde printe hele greia! ikke bare første arg
    str <- stringifyValue value False -- map over hele greia liksom og concat s og den nye lista
    modify (\s -> s { output = output s ++ [str] })
    return $ Number 1 -- mock value, maybe I should make Void a value type and return that instead. evt endre typen til (Either () Value)

callFunction (FunctionCall "length" args) = do
    maybeList <- evalExpr $ head args
    case maybeList of
        (List l) -> return (Number (toInteger $ length l))
        -- legg til hashmap og sånt og da
        _ -> throwError $ BadArgument "length can only be called with iterable!"

callFunction (FunctionCall name args) = do
    argsValues <- mapM evalExpr args
    function <- lookupFunc name
    case function of
        Nothing -> throwError $ FunctionNotFound name
        Just func -> applyFunction func argsValues

applyFunction :: Function -> [Value] -> Psnodig Value
applyFunction (Function _ args stmts) values = do
    when (length args /= length values) $ throwError $ WrongNumberOfArguments "Function takes fewer or more args than provided!"
    pushScope
    zipWithM_ bindVar (map fstArg args) values
    result <- evalStmts stmts
    popScope
    case result of
        Left _ -> throwError $ NoReturnError "Function must return something!"
        Right res -> return res


-- Main functions for execution

evalProgram :: Program -> Psnodig ()
evalProgram (Program structs funcs entryPoint) = do
    mapM_ processStructDecls structs
    mapM_ processFunDecl funcs
    void $ callFunction entryPoint
  where
    processFunDecl f@(Function name _ _) = bindFunc name f

initialState :: ExecutionState
initialState = ExecutionState
    { structDecls = Map.empty
    , structEnv = Map.empty
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

deref :: Value -> [Expression]
deref (List xs) = xs

stringifyValue :: Value -> Bool -> Psnodig String
stringifyValue v fromList = case v of
    Text t -> if fromList then return $ '\"' : t ++ "\"" else return t
    Nil -> return "\"Nil\""
    Number n ->  return $ show n
    Boolean b -> return $ show b
    HashSet s -> return $ show s
    HashMap m -> return $ show m
    List l -> stringifyList l
    StructVal _ -> return "this is a struct :D"

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
                nextVal <- evalExpr $ list !! fromInteger n
                evalNestedIndex nextVal indexExprs
            else throwError $ BadArgument "List index out of range!"
        _ -> throwError $ BadArgument "Index must evaluate to a number"
evalNestedIndex _ _ = throwError $ BadArgument "Expected a list for index operation"

-- replaceValueAtIndex :: [Expression] -> Int -> Value -> Psnodig (Maybe [Expression])
-- replaceValueAtIndex list index newVal =
--     case newVal of
--         (StructVal (Struct maybeStruct exprs)) -> do
--             maybeStructDecl <- lookupStructDecl maybeStruct
--             case maybeStructDecl of
--                 Just fields -> do
--                     values <- mapM evalExpr exprs
--                     valueToBeAdded = (Constant (StructVal $ zip fields values))
--                     if length list <= index then [valueToBeAdded]
--                     else let (before, _ : after) = splitAt index list
--                     in Just $ before ++ valueToBeAdded : after
--                 _ -> return $ Nothing
--         _ -> if length list <= index then [(Constant newVal)]
--              else let (before, _ : after) = splitAt index list
--              in Just $ before ++ (Constant newVal) : after

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