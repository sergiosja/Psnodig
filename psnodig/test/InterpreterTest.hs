module InterpreterTest (testInterpreter) where

import Syntax
import Interpreter
    ( runPsnodig
    , RuntimeError(..)
    , ExecutionState(..)
    , evalStmt
    , evalExpr
    , processStructDecls
    , stringifyValue
    , bval
    , callFunction
    )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Test.HUnit

defaultState :: ExecutionState
defaultState = ExecutionState
    { structDecls = Map.empty
    , funcDecls = Map.empty
    , scopeStack = [[[]]]
    , output = []
    }

s :: Int -> StructDecl
s 1 = StructDecl "Queue" [Argument "queue" "list", Argument "currpos" "int"]
s 2 = StructDecl "Person" [Argument "age" "int", Argument "job" "str"]
s _ = StructDecl "Default" []

f :: Int -> FunctionDecl
f 1 = FunctionDecl "f" [Argument "x" "int", Argument "y" "int"] [CallStmt (FunctionCall "print" [BinaryExp Plus (VariableExp "x") (VariableExp "y")]), Return (Constant (Number 1))]
f 2 = FunctionDecl "f" [] [Assignment (VariableTarget "s") (ExpressionValue (Constant (HashSet (Set.fromList [])))),CallStmt (FunctionCall "add" [Constant (Number 5),VariableExp "s"]),CallStmt (FunctionCall "add" [Constant (Number 5),VariableExp "s"]),CallStmt (FunctionCall "print" [VariableExp "s"]),Assignment (VariableTarget "m") (ExpressionValue (Constant (HashMap (Map.fromList [])))),CallStmt (FunctionCall "add" [Constant (Text "key1"),Constant (Text "val1"),VariableExp "m"]),CallStmt (FunctionCall "add" [Constant (Text "key2"),Constant (Text "val2"),VariableExp "m"]),CallStmt (FunctionCall "print" [VariableExp "m"]),Return (Constant (Number 1))]
f 3 = FunctionDecl "f" [] [Assignment (VariableTarget "bigL") (ExpressionValue (Constant (List [Constant (Number 1),Constant (Number 2),Constant (Number 3),Constant (Number 4),Constant (Number 5)]))),Loop (CallExp (FunctionCall "notEmpty" [VariableExp "bigL"])) [CallStmt (FunctionCall "print" [CallExp (FunctionCall "pop" [VariableExp "bigL"])])],Return (VariableExp "bigL")]
f 4 = FunctionDecl "notEmpty" [Argument "l" "list"] [Return (BinaryExp GreaterThan (CallExp (FunctionCall "length" [VariableExp "l"])) (Constant (Number 0)))]
f 5 = FunctionDecl "f" [] [Assignment (VariableTarget "q") (StructValue (Struct "Queue" [Constant (List []),Constant (Number 0)])),For "i" (Constant (Number 5)) (Constant (Number 20)) [CallStmt (FunctionCall "append" [VariableExp "i",StructFieldExp (StructField (VariableExp "q") (VariableExp "queue"))])],For "i" (Constant (Number 0)) (Constant (Number 10)) [Assignment (VariableTarget "tmp") (ExpressionValue (CallExp (FunctionCall "popQ" [VariableExp "q"]))),CallStmt (FunctionCall "print" [ListIndex "tmp" [Constant (Number 1)]]),Assignment (VariableTarget "q") (ExpressionValue (ListIndex "tmp" [Constant (Number 0)]))],Return (VariableExp "q")]
f 6 = FunctionDecl "popQ" [Argument "q" "Queue"] [Assignment (VariableTarget "tbr") (ExpressionValue (StructFieldExp (StructField (VariableExp "q") (ListIndex "queue" [StructFieldExp (StructField (VariableExp "q") (VariableExp "currpos"))])))),Assignment (StructFieldTarget (StructField (VariableExp "q") (VariableExp "currpos"))) (ExpressionValue (BinaryExp Plus (StructFieldExp (StructField (VariableExp "q") (VariableExp "currpos"))) (Constant (Number 1)))),Return (Constant (List [VariableExp "q",VariableExp "tbr"]))]
f 7 = FunctionDecl "f" [] [Assignment (VariableTarget "k") (ExpressionValue (Constant (List [Constant (Number 1),Constant (Number 2),Constant (Number 3)]))),For "i" (Constant (Number 0)) (Constant (Number 3)) [CallStmt (FunctionCall "print" [ListIndex "k" [VariableExp "i"]])]]
f 8 = FunctionDecl "f" [] [Assignment (VariableTarget "p") (StructValue (Struct "Person" [Constant (Number 25),Constant (Text "student")])),Assignment (StructFieldTarget (StructField (VariableExp "p") (StructFieldExp (StructField (VariableExp "job") (VariableExp "boss"))))) (ExpressionValue (Constant (Text "Diego")))]
f 9 = FunctionDecl "f" [Argument "x" "int"] [CallStmt (FunctionCall "print" [VariableExp "x"])]
f _ = FunctionDecl "default" [] []


testInterpreter :: Test
testInterpreter = TestList
    [ testStatements
    , testStatementsNegative
    , testStructDecls
    , testBinaryExp
    , testExpressionsNegative
    , testConstants
    , testCustomToString
    , testBval
    , testFunctionCall
    , testProgram
    ]


testProgram :: Test
testProgram = TestList
    [ "execute program, function with parameters" ~: do
        res <- runPsnodig (Program Nothing
                                   []
                                   [f 1]
                                   (Just (FunctionCall "f" [Constant (Number 1), Constant (Number 2)])))
        case res of
            Right res' -> res' @?= ExecutionState
                { structDecls = Map.fromList []
                , funcDecls = Map.fromList [("f", f 1)]
                , scopeStack = []
                , output = ["3"]
                }
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute program, adding values to hashset and hashmap" ~: do
        res <- runPsnodig (Program Nothing
                                   []
                                   [f 2]
                                   (Just (FunctionCall "f" [])))
        case res of
            Right res' -> res' @?= ExecutionState
                { structDecls = Map.fromList []
                , funcDecls = Map.fromList [("f", f 2)]
                , scopeStack = []
                , output = ["(5)", "{key1: val1, key2: val2}"]
                }
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute program, popping list in a loop" ~: do
            res <- runPsnodig (Program Nothing
                                    []
                                    [f 3, f 4]
                                    (Just (FunctionCall "f" [])))
            case res of
                Right res' -> res' @?= ExecutionState
                    { structDecls = Map.fromList []
                    , funcDecls = Map.fromList [("f", f 3), ("notEmpty", f 4)]
                    , scopeStack = []
                    , output = ["5", "4", "3", "2", "1"]
                    }
                Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute program with struct, loops, apppend, accessing struct fields" ~: do
            res <- runPsnodig (Program Nothing
                                    [s 1]
                                    [f 5, f 6]
                                    (Just (FunctionCall "f" [])))
            case res of
                Right res' -> res' @?= ExecutionState
                    { structDecls = Map.fromList [("Queue", ["queue", "currpos"])]
                    , funcDecls = Map.fromList [("f", f 5), ("popQ", f 6)]
                    , scopeStack = []
                    , output = ["5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"]
                    }
                Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute program with outofbounds error" ~: do
            res <- runPsnodig (Program Nothing
                                    []
                                    [f 7]
                                    (Just (FunctionCall "f" [])))
            case res of
                Left err -> err @?= RuntimeErrorWithOutput ["1", "2", "3"] (OutOfBounds "Attempted to access index 3 of list with length 3.")
                Right _ -> assertFailure "Expected FunctionNotFound error, but computation succeeded."

    , "execute program with invalid struct field error" ~: do
            res <- runPsnodig (Program Nothing
                                    [s 2]
                                    [f 8]
                                    (Just (FunctionCall "f" [])))
            case res of
                Left err -> err @?= RuntimeErrorWithOutput [] (InvalidStructField "job does not contain struct fields.")
                Right _ -> assertFailure "Expected FunctionNotFound error, but computation succeeded."

    , "execute program with invalid wrong number of arguments error" ~: do
            res <- runPsnodig (Program Nothing
                                    []
                                    [f 8]
                                    (Just (FunctionCall "f" [Constant (Decimal 12.34)])))
            case res of
                Left err -> err @?= RuntimeErrorWithOutput [] (WrongNumberOfArguments "Function 'f' takes a different number of args than provided!")
                Right _ -> assertFailure "Expected FunctionNotFound error, but computation succeeded."

    , "execute program with no return" ~: do
            res <- runPsnodig (Program Nothing
                                    []
                                    [f 9]
                                    (Just (FunctionCall "f" [Constant (Number 59)])))
            case res of
                Left err -> err @?= RuntimeErrorWithOutput ["59"] (NoReturnError "Function 'f' must return something!")
                Right _ -> assertFailure "Expected FunctionNotFound error, but computation succeeded."

    , "execute program with no function call" ~: do
            res <- runPsnodig (Program Nothing
                                    []
                                    [f 9]
                                    Nothing)
            case res of
                Left err -> err @?= MissingEntryPoint "No function call located to run your program."
                Right _ -> assertFailure "Expected MissingEntryPoint error, but computation succeeded."
    ]


runCustomToString :: Value -> IO (Either RuntimeError (String, ExecutionState))
runCustomToString value =
    runExceptT $ runStateT (stringifyValue value False) defaultState

testCustomToString :: Test
testCustomToString = TestList
    [ "tostring nil" ~: do
        res <- runCustomToString Nil
        case res of
            Right (res', _) -> res' @?= "Nil"
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "tostring boolean" ~: do
        res <- runCustomToString (Boolean True)
        case res of
            Right (res', _) -> res' @?= "True"
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "tostring number" ~: do
        res <- runCustomToString (Number 56)
        case res of
            Right (res', _) -> res' @?= "56"
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "tostring decimal" ~: do
        res <- runCustomToString (Decimal 2.18)
        case res of
            Right (res', _) -> res' @?= "2.18"
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "tostring text" ~: do
        res <- runCustomToString (Text "posso avere un'altra birra per favore")
        case res of
            Right (res', _) -> res' @?= "posso avere un'altra birra per favore"
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "tostring list" ~: do
        res <- runCustomToString (List [(Constant (Number 1)), (Constant (Decimal 2.5)), (Constant (Boolean False)), (Constant Nil)])
        case res of
            Right (res', _) -> res' @?= "[1, 2.5, False, Nil]"
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "tostring hashset" ~: do
        res <- runCustomToString (HashSet (Set.fromList [(Constant (Decimal 9.9)), (Constant (Decimal 9.9)), (Constant (Decimal 9.9))]))
        case res of
            Right (res', _) -> res' @?= "(9.9)"
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "tostring hashmap" ~: do
        res <- runCustomToString (HashMap (Map.fromList [(Constant (Number 1), Constant (Text "value")), (Constant (Decimal 2.5), Constant (Number 123))]))
        case res of
            Right (res', _) -> res' @?= "{1: value, 2.5: 123}"
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "tostring structval" ~: do
        res <- runCustomToString (StructVal [("toni", (Decimal 4.7)), ("aron", (Number 26)), ("hector", (Text "yabba")), ("kdot", (List [(Constant (Number 40))]))])
        case res of
            Right (res', _) -> res' @?= "toni: 4.7, aron: 26, hector: yabba, kdot: [40]"
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err
    ]

testBval :: Test
testBval = TestList
    [ "bval nil"
        ~: let res = bval Nil
           in res
        ~?= False

    , "bval positive number"
        ~: let res = bval (Number 68)
           in res
        ~?= True

    , "bval negative number"
        ~: let res = bval (Number (- 5))
           in res
        ~?= False

    , "bval positive decimal"
        ~: let res = bval (Decimal 0.00001)
           in res
        ~?= True

    , "bval negative decimal"
        ~: let res = bval (Decimal 0.0)
           in res
        ~?= False

    , "bval empty text"
        ~: let res = bval (Text "")
           in res
        ~?= False

    , "bval text"
        ~: let res = bval (Text "siamo ragazzi di palermo")
           in res
        ~?= True

    , "bval empty list"
        ~: let res = bval (List [])
           in res
        ~?= False

    , "bval list"
        ~: let res = bval (List [(Constant (Text "ciao bonzo"))])
           in res
        ~?= True

    , "bval empty hashset"
        ~: let res = bval (HashSet (Set.fromList []))
           in res
        ~?= False

    , "bval hashset"
        ~: let res = bval (HashSet (Set.fromList [(Constant (Number 1)), (Constant (Number 1)), (Constant (Number 1))]))
           in res
        ~?= True

    , "bval empty hashmap"
        ~: let res = bval (HashMap (Map.fromList []))
           in res
        ~?= False

    , "bval hashmap"
        ~: let res = bval (HashMap (Map.fromList [(Constant (Number 1), Constant (Text "value")), (Constant (Decimal 2.5), Constant (Number 123))]))
           in res
        ~?= True
    ]


runFunction :: FunctionCall -> IO (Either RuntimeError (Value, ExecutionState))
runFunction call =
    runExceptT $ runStateT (callFunction call) defaultState

testFunctionCall :: Test
testFunctionCall = TestList
    [ "execute floor" ~: do
        res <- runFunction (FunctionCall "floor" [Constant (Decimal 14.999)])
        case res of
            Right (res', _) -> res' @?= Number 14
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute ceil" ~: do
        res <- runFunction (FunctionCall "ceil" [Constant (Decimal 15.000003)])
        case res of
            Right (res', _) -> res' @?= Number 16
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute max" ~: do
        res <- runFunction (FunctionCall "max" [Constant (Decimal 14.999), Constant (Number 21), Constant (Decimal 21.00001)])
        case res of
            Right (res', _) -> res' @?= Decimal 21.00001
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute min" ~: do
        res <- runFunction (FunctionCall "min" [Constant (Decimal 14.999), Constant (Number 21), Constant (Decimal 21.00001)])
        case res of
            Right (res', _) -> res' @?= Decimal 14.999
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute in" ~: do
        res <- runFunction (FunctionCall "in" [Constant (Decimal 14.999), Constant (List [Constant (Decimal 14.999)])])
        case res of
            Right (res', _) -> res' @?= Boolean True
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute not in" ~: do
        res <- runFunction (FunctionCall "in" [Constant (Decimal 15), Constant (List [Constant (Decimal 14.999)])])
        case res of
            Right (res', _) -> res' @?= Boolean False
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute length of text" ~: do
        res <- runFunction (FunctionCall "length" [Constant (Text "lupo studentesco")])
        case res of
            Right (res', _) -> res' @?= Number 16
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute length of list" ~: do
        res <- runFunction (FunctionCall "length" [Constant (List [Constant (Number 5), Constant (List [])])])
        case res of
            Right (res', _) -> res' @?= Number 2
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute length of hashset" ~: do
        res <- runFunction (FunctionCall "length" [Constant (HashSet (Set.fromList [Constant (Text ""), Constant (Text ""), Constant (Text "")]))])
        case res of
            Right (res', _) -> res' @?= Number 1
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute length of hashmap" ~: do
        res <- runFunction (FunctionCall "length" [Constant (HashMap (Map.fromList [(Constant (Text "key"), Constant (Text "value")), (Constant (Text "goals"), Constant (Number 123))]))])
        case res of
            Right (res', _) -> res' @?= Number 2
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute print" ~: do
        res <- runFunction (FunctionCall "print" [Constant (Number 1907), Constant (Text "Como")])
        case res of
            Right (res', _) -> res' @?= Number 2
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    ]


runStructDecl :: StructDecl -> IO (Either RuntimeError ((), ExecutionState))
runStructDecl struct =
    runExceptT $ runStateT (processStructDecls struct) defaultState

testStructDecls :: Test
testStructDecls = TestList
    [ "execute empty struct" ~: do
        res <- runStructDecl (StructDecl "Person" [])
        case res of
            Right (res', _) -> res' @?= ()
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute small struct" ~: do
        res <- runStructDecl (StructDecl "Person" [Argument "age" "int"])
        case res of
            Right (res', _) -> res' @?= ()
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute large struct" ~: do
        res <- runStructDecl (StructDecl "Person" [Argument "name" "str", Argument "age" "int", Argument "city" "str", Argument "friends" "list", Argument "children" "int"])
        case res of
            Right (res', _) -> res' @?= ()
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err
    ]


runStmt :: Statement -> IO (Either RuntimeError (Either () Value, ExecutionState))
runStmt stmt =
    runExceptT $ runStateT (evalStmt stmt) defaultState

testStatements :: Test
testStatements = TestList
    [ "execute assignment with variable target" ~: do
        res <- runStmt (Assignment (VariableTarget "var") (ExpressionValue (Constant (Decimal 20.24))))
        case res of
            Right (res', _) -> res' @?= Left ()
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute while loop" ~: do
        res <- runStmt (Loop (BinaryExp Equal (Constant (Number 0)) (Constant (Number 10)))
                       [Assignment (VariableTarget "x") (ExpressionValue (BinaryExp Plus (Constant (Number 1)) (Constant (Number 2))))])
        case res of
            Right (res', _) -> res' @?= Left ()
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute if without else" ~: do
        res <- runStmt (If (BinaryExp Equal (Constant (Number 10)) (Constant (Number 10)))
                       [Return (Constant (Boolean True))] Nothing)
        case res of
            Right (res', _) -> res' @?= Right (Boolean True)
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute if with else if" ~: do
        res <- runStmt (If (BinaryExp Equal (Constant (Number 10)) (Constant (Number 10)))
                       [Return (Constant (Boolean True))]
                       (Just (ElseIf (Constant (Boolean True)) [Return (Constant (Boolean False))] Nothing)))
        case res of
            Right (res', _) -> res' @?= Right (Boolean True)
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute if with else" ~: do
        res <- runStmt (If (BinaryExp Equal (Constant (Number 10)) (Constant (Number 10)))
                       [Return (Constant (Boolean True))]
                       (Just (Else [Return (Constant (Boolean False))])))
        case res of
            Right (res', _) -> res' @?= Right (Boolean True)
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute foreach loop" ~: do
        res <- runStmt (ForEach "num"
                       (Constant (List [(Constant (Number 1)), (Constant (Number 2)), (Constant (Number 3)), (Constant (Number 4))]))
                       [CallStmt (FunctionCall "print" [VariableExp "num"])])
        case res of
            Right (res', _) -> res' @?= Left ()
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute for loop" ~: do
        res <- runStmt (For "index" (Constant (Number 0)) (Constant (Number 10))
                       [CallStmt (FunctionCall "print" [VariableExp "index"])])
        case res of
            Right (res', _) -> res' @?= Left ()
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute returning number" ~: do
        res <- runStmt (Return (Constant (Number 20)))
        case res of
            Right (res', _) -> res' @?= Right (Number 20)
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute returning list" ~: do
        res <- runStmt (Return (Constant (List [Constant (Number 1), Constant (Number 2)])))
        case res of
            Right (res', _) -> res' @?= Right (List [Constant (Number 1), Constant (Number 2)])
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute hash stmt" ~: do
        res <- runStmt (HashStmt (Return (Constant (Number 50))))
        case res of
            Right (res', _) -> res' @?= Right (Number 50)
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute annotation stmt" ~: do
        res <- runStmt (AnnotationStmt "p <- choosePivot(liste)" [Assignment (VariableTarget "p") (ExpressionValue (Constant (Number 0)))])
        case res of
            Right (res', _) -> res' @?= Left ()
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err
    ]

testStatementsNegative :: Test
testStatementsNegative = TestList
    [ "execute assignment with undefined struct" ~: do
        res <- runStmt (Assignment (ListIndexTarget "people" [Constant (Number 20)]) (StructValue (Struct "Person" [Constant (Text "Giuliano"), Constant (Number 70)])))
        case res of
            Right _ -> assertFailure "Expected StructNotFound error, but computation succeeded."
            Left err -> err @?= RuntimeErrorWithOutput [] (StructNotFound "Struct Person is not defined.")
    , "execute assignment with undefined variable" ~: do
        res <- runStmt (Assignment (VariableTarget "var") (ExpressionValue (VariableExp ("x"))))
        case res of
            Right _ -> assertFailure "Expected StructNotFound error, but computation succeeded."
            Left err -> err @?= RuntimeErrorWithOutput [] (VariableNotFound "Variable x not found.")
    ]


runExpr :: Expression -> IO (Either RuntimeError (Value, ExecutionState))
runExpr expr =
    runExceptT $ runStateT (evalExpr expr) defaultState

testBinaryExp :: Test
testBinaryExp = TestList
    [ "execute plus" ~: do
        res <- runExpr (BinaryExp Plus (Constant (Number 10)) (Constant (Number 10)))
        case res of
            Right (res', _) -> res' @?= Number 20
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute minus" ~: do
        res <- runExpr (BinaryExp Minus (Constant (Number 20)) (Constant (Number 10)))
        case res of
            Right (res', _) -> res' @?= Number 10
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute times" ~: do
        res <- runExpr (BinaryExp Times (Constant (Number 3)) (Constant (Number 10)))
        case res of
            Right (res', _) -> res' @?= Number 30
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute times with decimal and integer" ~: do
        res <- runExpr (BinaryExp Times (Constant (Decimal 2.5)) (Constant (Number 7)))
        case res of
            Right (res', _) -> res' @?= Decimal 17.5
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute division" ~: do
        res <- runExpr (BinaryExp Division (Constant (Number 10)) (Constant (Number 2)))
        case res of
            Right (res', _) -> res' @?= Number 5
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute less than" ~: do
        res <- runExpr (BinaryExp LessThan (Constant (Number 10)) (Constant (Number 30)))
        case res of
            Right (res', _) -> res' @?= Boolean True
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute less than or equal" ~: do
        res <- runExpr (BinaryExp LessThanEqual (Constant (Number 19)) (Constant (Number 10)))
        case res of
            Right (res', _) -> res' @?= Boolean False
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute greater than" ~: do
        res <- runExpr (BinaryExp GreaterThan (Constant (Number 9)) (Constant (Number 10)))
        case res of
            Right (res', _) -> res' @?= Boolean False
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute greater than or equal" ~: do
        res <- runExpr (BinaryExp GreaterThanEqual (Constant (Number 90)) (Constant (Number 90)))
        case res of
            Right (res', _) -> res' @?= Boolean True
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute equal" ~: do
        res <- runExpr (BinaryExp Equal (Constant (Number 4)) (Constant (Number 6)))
        case res of
            Right (res', _) -> res' @?= Boolean False
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute not equal" ~: do
        res <- runExpr (BinaryExp NotEqual (Constant (Number 11)) (Constant (Number 9)))
        case res of
            Right (res', _) -> res' @?= Boolean True
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute modulo" ~: do
        res <- runExpr (BinaryExp Modulo (Constant (Number 142)) (Constant (Number 29)))
        case res of
            Right (res', _) -> res' @?= Number 26
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute true and" ~: do
        res <- runExpr (BinaryExp And (Constant (Boolean True)) (Constant (Boolean True)))
        case res of
            Right (res', _) -> res' @?= Boolean True
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute false and" ~: do
        res <- runExpr (BinaryExp And (Constant (Boolean True)) (Constant (Boolean False)))
        case res of
            Right (res', _) -> res' @?= Boolean False
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute true or" ~: do
        res <- runExpr (BinaryExp Or (Constant (Boolean False)) (Constant (Boolean True)))
        case res of
            Right (res', _) -> res' @?= Boolean True
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute false or" ~: do
        res <- runExpr (BinaryExp Or (Constant (Boolean False)) (Constant (Boolean False)))
        case res of
            Right (res', _) -> res' @?= Boolean False
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute true or using bval" ~: do
        res <- runExpr (BinaryExp Or (Constant (Boolean (bval $ Number 1))) (Constant (Boolean (bval $ Decimal 0.1))))
        case res of
            Right (res', _) -> res' @?= Boolean True
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err
    ]

testExpressionsNegative :: Test
testExpressionsNegative = TestList
    [ "execute function call without declaring function" ~: do
        res <- runExpr (CallExp (FunctionCall "f" []))
        case res of
            Left err -> err @?= RuntimeErrorWithOutput [] (FunctionNotFound "f function is not previously declared. Check the spelling.")
            Right _ -> assertFailure "Expected FunctionNotFound error, but computation succeeded."

    , "execute list index access without declaring list" ~: do
        res <- runExpr (ListIndex "list" [Constant (Number 1)])
        case res of
            Left err -> err @?= RuntimeErrorWithOutput [] (ListNotFound "List list not found.")
            Right _ -> assertFailure "Expected FunctionNotFound error, but computation succeeded."

    , "execute argument mismatch" ~: do
        res <- runExpr (BinaryExp Minus (Constant (Text "hei")) (Constant (Number 10)))
        case res of
            Left err -> err @?= RuntimeErrorWithOutput [] (ArithmeticError "Incompatible operands! Tried to apply Minus to Text \"hei\" and Number 10!")
            Right _ -> assertFailure "Expected FunctionNotFound error, but computation succeeded."

    , "execute division by zero" ~: do
        res <- runExpr (BinaryExp Division (Constant (Number 42)) (Constant (Number 0)))
        case res of
            Left err -> err @?= RuntimeErrorWithOutput [] (ArithmeticError "Division by zero!")
            Right _ -> assertFailure "Expected FunctionNotFound error, but computation succeeded."

    , "execute modulo by zero" ~: do
        res <- runExpr (BinaryExp Modulo (Constant (Number 2312)) (Constant (Number 0)))
        case res of
            Left err -> err @?= RuntimeErrorWithOutput [] (ArithmeticError "Modulo by zero!")
            Right _ -> assertFailure "Expected FunctionNotFound error, but computation succeeded."

    , "execute modulo with decimal" ~: do
        res <- runExpr (BinaryExp Modulo (Constant (Decimal 9.9)) (Constant (Number 3)))
        case res of
            Left err -> err @?= RuntimeErrorWithOutput [] (ArithmeticError "Avoid using modulo with decimals!")
            Right _ -> assertFailure "Expected FunctionNotFound error, but computation succeeded."
    ]

testConstants :: Test
testConstants = TestList
    [ "execute small number" ~: do
        res <- runExpr (Constant (Number 5))
        case res of
            Right (value, _) -> value @?= Number 5
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute big number" ~: do
        res <- runExpr (Constant (Number 42174871982315980219241))
        case res of
            Right (value, _) -> value @?= Number 42174871982315980219241
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute nil" ~: do
        res <- runExpr (Constant Nil)
        case res of
            Right (value, _) -> value @?= Nil
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute positive boolean" ~: do
        res <- runExpr (Constant (Boolean True))
        case res of
            Right (value, _) -> value @?= Boolean True
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute negative boolean" ~: do
        res <- runExpr (Constant (Boolean False))
        case res of
            Right (value, _) -> value @?= Boolean False
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute decimal" ~: do
        res <- runExpr (Constant (Decimal 3.14))
        case res of
            Right (value, _) -> value @?= Decimal 3.14
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute empty text" ~: do
        res <- runExpr (Constant (Text ""))
        case res of
            Right (value, _) -> value @?= Text ""
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute regular text" ~: do
        res <- runExpr (Constant (Text "informatics"))
        case res of
            Right (value, _) -> value @?= Text "informatics"
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute text with spaces" ~: do
        res <- runExpr (Constant (Text "I love informatics"))
        case res of
            Right (value, _) -> value @?= Text "I love informatics"
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute empty list" ~: do
        res <- runExpr (Constant (List []))
        case res of
            Right (value, _) -> value @?= List []
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute list with numbers" ~: do
        res <- runExpr (Constant (List [Constant (Number 1), Constant (Number 2), Constant (Number 3)]))
        case res of
            Right (value, _) -> value @?= List [Constant (Number 1), Constant (Number 2), Constant (Number 3)]
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute nested list" ~: do
        res <- runExpr (Constant (List [Constant (Number 1), Constant (List [Constant (List [Constant (Number 2)]), Constant (List [Constant (Number 3)])])]))
        case res of
            Right (value, _) -> value @?= List [Constant (Number 1), Constant (List [Constant (List [Constant (Number 2)]), Constant (List [Constant (Number 3)])])]
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute empty hashset" ~: do
        res <- runExpr (Constant (HashSet (Set.fromList [])))
        case res of
            Right (value, _) -> value @?= HashSet (Set.fromList [])
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute hashset" ~: do
        res <- runExpr (Constant (HashSet (Set.fromList [Constant (Number 1), Constant (Number 2), Constant (Number 1), Constant (Number 2), Constant (Number 1), Constant (Number 2)])))
        case res of
            Right (value, _) -> value @?= HashSet (Set.fromList [Constant (Number 1), Constant (Number 2), Constant (Number 1), Constant (Number 2), Constant (Number 1), Constant (Number 2)])
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute empty hashmap" ~: do
        res <- runExpr (Constant (HashMap (Map.fromList [])))
        case res of
            Right (value, _) -> value @?= HashMap (Map.fromList [])
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute hashmap with string keys" ~: do
        res <- runExpr (Constant (HashMap (Map.fromList [(Constant (Text "key"), Constant (Text "value")), (Constant (Text "goals"), Constant (Number 123))])))
        case res of
            Right (value, _) -> value @?= HashMap (Map.fromList [(Constant (Text "key"), Constant (Text "value")), (Constant (Text "goals"), Constant (Number 123))])
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err

    , "execute hashmap with different type keys" ~: do
        res <- runExpr (Constant (HashMap (Map.fromList [(Constant (Number 1), Constant (Text "value")), (Constant (Decimal 2.5), Constant (Number 123))])))
        case res of
            Right (value, _) -> value @?= HashMap (Map.fromList [(Constant (Number 1), Constant (Text "value")), (Constant (Decimal 2.5), Constant (Number 123))])
            Left err -> assertFailure $ "Runtime error during evaluation: " ++ show err
    ]
