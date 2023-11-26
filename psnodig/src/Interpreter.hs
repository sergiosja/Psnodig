module Interpreter (ExecutionState(..), runPsnodig) where

import Syntax
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (void)

type StructDecls = [(String, [String])] -- (Person, [age, dob])
type StructEnv = [(String, [Struct])] -- (p, (Person, [1, 2]))
type FuncEnv = [(String, Function)]
type Scope = [(String, Value)]
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
    | ArithmeticError String -- e.g. division by zero and stuff
    | BadArgument String -- e.g. array["index"]
    | WrongNumberOfArguments String
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
    currScope@(ExecutionState { scopeStack = (_ : scopes)} ) <- get
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
    currScope@(ExecutionState { scopeStack = (top:rest)} ) <- get
    put currScope { scopeStack = ((var, value):top) : rest }

lookupFunc :: String -> Psnodig (Maybe Function)
lookupFunc func = do
    ExecutionState { funcEnv = env } <- get
    return $ lookup func env

bindFunc :: String -> Function -> Psnodig ()
bindFunc name func = do
    currScope@(ExecutionState { funcEnv = env }) <- get
    put currScope { funcEnv = (name, func):env }

lookupStructDecl :: String -> Psnodig (Maybe [Argument])
lookupStructDecl struct = do
    ExecutionState { structDecls = env } <- get
    return $ lookup struct env





-- Save (Person, [age, dob]) to structDecls
saveStructDecls :: String -> [Argument] -> Psnodig ()
saveStructDecls name args = do
    currScope@(ExecutionState { structDecls = env }) <- get
    put currScope { structDecls = (name, args) : env }

-- bruh bare tegn på tavla så finner du ut

-- -- (p, (Person, [1, 2]))
-- -- see if struct exists, if so, return whole mf
-- lookupStruct :: String -> Psnodig (Maybe Struct)
-- lookupStruct maybeStruct = do
--     ExecutionState { structEnv = scope } <- get
--     case lookup scope maybeStruct of
--         Just (Struct t args) -> Just (Struct struct args)
--         Nothing -> Nothing


-- -- on e.g. p := struct Person(1, 2)

-- -- bindStruct: Person (Struct p [1, 2])
-- bindStruct :: String -> Struct -> Either String Psnodig () -- (p, (Person, [1, 2]))
-- bindStruct maybeStruct s@(Struct name args) = do
--     ExecutionState { structDecls = scope } <- get

--     case lookup scope maybeStruct of
--         Just (StructDecl sname sArgs) -> do
--             currScope@(ExecutionState { structEnv = env }) <- get
--             if length args == length sArgs
--             then put currScope { structEnv = (name, s):env }
--             else Left "Struct must be initialised with same number of args!"
--         Nothing -> Left "Trying to initialise an undeclared struct!"


-- -- on e.g. p.age := 5
-- -- yields error if struct or field doesnt exist
-- bindStructField :: StructField -> Either String Psnodig ()
-- bindStructField (StructField name val) = do
--     ExecutionState { structDecls  }








-- Expressions

evalExpr :: Expression -> Psnodig Value
evalExpr (Constant v) = return v
evalExpr (VariableExp var) = do
    maybeVar <- lookupVar var
    case maybeVar of
        Just val -> return val
        Nothing -> throwError $ VariableNotFound "Variable not found!"
evalExpr (BinaryExp op expr1 expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case operate op val1 val2 of
        Left errMsg -> throwError $ ArithmeticError errMsg
        Right val -> return val
evalExpr (ListIndex var expr) = do
    maybeList <- lookupVar var
    idx <- evalExpr expr
    case maybeList of
        Just val -> case val of
            (List list) -> case idx of
                (Number n) -> if fromInteger n > length list || fromInteger n < length list
                    then evalExpr $ list !! fromInteger n else throwError $ BadArgument "List index out of range!"
                _ -> throwError $ BadArgument "List index must evaluate to number"
            _ -> throwError $ BadArgument "Left hand side of expression is not list!"
        Nothing -> throwError $ VariableNotFound "Variable not found!"
evalExpr (CallExp fcall) = callFunction fcall
evalExpr (Not expr) = (Boolean . not . bval) <$> evalExpr expr

evalStructField :: StructField -> Psnodig Value
evalStructField (StructField name arg) = do
    -- st <- exists?
    -- exists ->
        -- does field exist on st?
        -- exists -> 
        -- no gooda -> bad
    -- no gooda -> bad


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

evalStmts :: [Statement] -> Psnodig Value
evalStmts ((Return expr):_) = evalExpr expr
evalStmts (stmt:stmts) = do
    result <- evalStmt stmt
    case result of
        Left _ -> evalStmts stmts
        Right value -> return value

evalStmt :: Statement -> Psnodig (Either () Value)
evalStmt (Return expr) = Right <$> evalExpr expr
evalStmt _ = return (Left ())

-- Function

callFunction :: FunctionCall -> Psnodig Value
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
    return result

-- Main functions for execution

evalProgram :: Program -> Psnodig ()
evalProgram (Program structDecls funDecls entryPoint) = do
    mapM_ processDecl structDecls
    mapM_ processFunDecl funDecls
    void $ callFunction entryPoint
  where
    processDecl (StructDecl name args) = saveStructDecls name args
    processFunDecl f@(Function name _ _) = bindFunc name f

initialState :: ExecutionState
initialState = ExecutionState
    { structDecls = []
    , structEnv = []
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

emit :: String -> Psnodig ()
emit s = do
    oldState <- get
    put $ oldState { output = s : output oldState }


-- data AssignmentTarget =
--       VariableTarget String             -- x =
--     | ListIndexTarget String Expression -- x[0] =
--     | StructFieldTarget StructField     -- x.left =
--     deriving (Eq, Show, Read, Ord)

-- data AssignmentValue =
--       ExpressionValue Expression    -- = 5 + 5
--     | StructValue Struct  -- = struct Person
--     deriving (Eq, Show, Read, Ord)












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

-- Environments
-- type StructEnvironment = [(String, [Argument])] -- Map.Map String [Argument]
-- type FuncEnvironment = [(String, Function)] -- Map.Map String Function
-- type ValueEnvironment = [(String, Value)] -- Map.Map String Value

-- type Output = [String]
-- type ErrorMsg = String

-- data RuntimeError =
--       VariableNotFound ErrorMsg
--     | FunctionNotFound ErrorMsg
--     | Error ErrorMsg
--     deriving (Eq, Show)

-- data GourmetEnvironment = GourmetEnvironment
--     { structDecls :: StructEnvironment
--     , funcEnv :: FuncEnvironment
--     , valueEnv :: ValueEnvironment
--     }

-- initialEnvironment :: GourmetEnvironment
-- initialEnvironment = GourmetEnvironment { structDecls = [], funcEnv = [], valueEnv = [] }

-- -- type Runtime a = GourmetEnvironment -> (Either RuntimeError a, Output)
-- -- newtype Gourmet a = Gourmet { run :: Runtime a }

-- -- instance Monad Gourmet where
-- --     return a = Gourmet $ \_ -> (return a, [])
-- --     p >>= f = Gourmet $ \env ->
-- --         case run p env of
-- --             (Left err, out) -> (Left err, out)
-- --             (Right a, out1) ->
-- --                 let (res, out2) = run (f a) env
-- --                 in (res, out1 ++ out2)

-- -- instance Functor Gourmet where
-- --   fmap = liftM

-- -- instance Applicative Gourmet where
-- --   pure = return
-- --   (<*>) = ap


-- -- -- Gourmet monad operations
-- -- abort :: RuntimeError -> Gourmet a
-- -- abort err = Gourmet $ \_ -> (Left err, mempty)

-- -- bindValue :: String -> Value -> (Gourmet a -> Gourmet a)
-- -- bindValue var value (Gourmet gourmet) = Gourmet $ \env ->
-- --     let newValueEnv = Map.insert var value (valueEnv env)
-- --     in gourmet $ env { valueEnv = newValueEnv }

-- {- problemet her er at bind egt skal funke for ting som
--     x = 5 og h = Height(1, 2, 3)

--     når vi har bare struct Height { a int b int c int } etc., så har vi
--     ikke egentlig noen 
--  -}
-- -- bind :: String -> Value -> (Gourmet a -> Gourmet a)
-- -- bind var value (Gourmet gourmet) = Gourmet $ \env ->
-- --     case value of
-- --         (StructDecl name args) ->
-- --             let newStructEnv = Map.insert var args (structDecls env)
-- --             in gourmet $ env { structDecls = newStructEnv }
-- --         (Function name _ _) ->
-- --             let newFuncEnv = Map.insert var value
-- --         _ ->
-- --             let newValueEnv = Map.insert var value (valueEnv env)
-- --             in gourmet $ env { valueEnv = newValueEnv }

-- -- find :: String -> Gourmet Value
-- -- find x = Gourmet $ \env ->
-- --     case Map.lookup x (valueEnv env) of
-- --         Just v -> (Right v, [])
-- --         Nothing -> (Left (VariableNotFound x), [])

-- -- output :: String -> Gourmet ()
-- -- output s = Gourmet $ \_ -> (Right (), [s])

-- -- -- Expressions
-- -- eval :: Expression -> Gourmet Value
-- -- eval (Constant v) = return v
-- -- eval (VariableExp x) = find x
-- -- eval (BinaryExp op expr1 expr2) = do
-- --     v1 <- eval expr1
-- --     v2 <- eval expr2
-- --     case operate op v1 v2 of
-- --         Right v -> return v
-- --         Left err -> abort $ Error err
-- -- eval (ListIndex list expr) = undefined
-- -- eval (CallExp funcCall) = undefined
-- -- eval (Not expr) = undefined
-- -- eval (StructFieldExp structField) = undefined
-- -- eval _ = undefined

-- operate :: Operator -> Value -> Value -> Either ErrorMsg Value
-- operate Plus (Number x) (Number y) = return $ Number $ x + y
-- operate Minus (Number x) (Number y) = return $ Number $ x - y
-- operate Times (Number x) (Number y) = return $ Number $ x * y
-- operate Division (Number _) (Number 0) = Left "Division by 0"
-- operate Division (Number x) (Number y) = return $ Number $ div x y -- change to (/) and add float DT later
-- operate LessThan (Number x) (Number y) = return $ Boolean $ x < y
-- operate LessThanEqual (Number x) (Number y) = return $ Boolean $ x <= y
-- operate GreaterThan (Number x) (Number y) = return $ Boolean $ x > y
-- operate GreaterThanEqual (Number x) (Number y) = return $ Boolean $ x >= y
-- operate Equal (Number x) (Number y) = return $ Boolean $ x == y
-- operate NotEqual (Number x) (Number y) = return $ Boolean $ x /= y
-- operate And (Number x) (Number y) =
--     return $ Boolean $ if x > 0 && y > 0 then True else False
-- operate Or (Number x) (Number y) =
--     return $ Boolean $ if x > 0 || y > 0 then True else False
-- operate Modulo (Number _) (Number 0) = Left "Modulo by 0"
-- operate Modulo (Number x) (Number y) = return $ Number $ mod x y
-- operate _ _ _ = left "Only numbers can be applied to this function"

-- -- -- Functions (?)
-- -- apply :: String -> [String] -> Gourmet Value
-- -- apply x args = undefined


-- -- -- Statements
-- -- execute :: Statement -> Gourmet ()
-- -- execute (Assignment assTarget assValue) = undefined -- | Assignment AssignmentTarget AssignmentValue
-- -- execute (Loop expr stmts) = undefined -- | Loop Expression [Statement]
-- -- execute (If expr stmts maybeElse) = undefined -- | If Expression [Statement] (Maybe Else)
-- -- execute (ForEach ident expr stmts) = undefined -- | ForEach String Expression [Statement]
-- -- execute (For ident expr1 expr2 stmts) = undefined -- | For String Expression Expression [Statement]
-- -- execute (CallStmt func) = undefined -- | CallStmt FunctionCall
-- -- execute (Return expr) = undefined -- | Return Expression
-- -- execute (HashStmt stmt) = execute stmt -- | HashStmt Statement
-- -- execute (AnnotationStmt _ stmts) = undefined -- | AnnotationStmt String [Statement]
-- -- execute (Break) = undefined -- | Break
-- -- execute (Continue) = undefined -- | Continue

-- -- Executing

-- -- execProgram :: Program -> Gourmet ()
-- -- execProgram (Program _ [] _) = return ()
-- -- execProgram (Program structs funcs func) = do
-- --     mapM_ saveStruct structs
-- --     mapM_ saveFunc funcs
-- --     return ()
-- --     -- eval (BinaryExp Plus (Constant (Number 1)) (Constant (Number 2)))
-- -- --     runFunc func

-- -- saveStruct :: StructDecl -> Gourmet ()
-- -- saveStruct (StructDecl name args) = Gourmet $ \env ->
-- --     let newStructEnv = Map.insert name args (structDecls env)
-- --     in (Right (), [])

-- -- saveFunc :: Function -> Gourmet ()
-- -- saveFunc func@(Function name _ _) = Gourmet $ \env ->
-- --     let newFuncEnv = Map.insert name func (funcEnv env)
-- --     in (Right (), [])

-- -- runPsnodig :: Program -> (Output, Maybe RuntimeError)
-- -- runPsnodig program =
-- --   case run (execProgram program) initialEnvironment of
-- --     (Right _, prints) -> (prints, Nothing)
-- --     (Left e, prints) -> (prints, Just e)

-- -- data Program = Program [StructDecl] [Function] FunctionCall

{-
struct Person {
    age int,
    dob int
}

func f(x int) {
    return x
}

f(10)
-}


-- runPsnodig :: (Output, Maybe RuntimeError)
-- runPsnodig =
--   case run (execProgram prgrm) initialEnvironment of
--     (Right _, prints) -> (prints, Nothing)
--     (Left e, prints) -> (prints, Just e)
