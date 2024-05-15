module Writers.Python (testPythonWriter) where

import Syntax
import Python.PythonWriter
    ( writeValue
    , writeProgramDescription
    , writeExpr
    , writeStmt
    , writeClass
    , writePython
    )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Writer
import Test.HUnit


testPythonWriter :: Test
testPythonWriter = TestList
    [ testProgramDescription
    , testValues
    , testExpressions
    , testStatements
    , testStructDecl
    , testProgram
    ]

testProgram :: Test
testProgram = TestList
    [ "write empty program"
        ~: (execWriter $ writePython (Program Nothing [] [] Nothing))
        ~?= ""

    , "write program with just description"
        ~: (execWriter $ writePython (Program (Just (ProgramDescription "There is no program." "Thus nothing is returned.")) [] [] Nothing))
        ~?= "# Input: There is no program.\n# Output: Thus nothing is returned.\n\n"

    , "write program with just struct"
        ~: (execWriter $ writePython (Program Nothing [StructDecl "Person" [Argument "age" "int"]] [] Nothing))
        ~?= "class Person:\n\tdef __init__(self, age):\n\t\tself.age = age\n\n"

    , "write program with just function declaration"
        ~: (execWriter $ writePython (Program Nothing [] [FunctionDecl "f" [] [Return (Constant (Number 1))]] Nothing))
        ~?= "def f():\n\treturn 1\n\n"

    , "write simple program"
        ~: (execWriter $ writePython (Program Nothing
                                                        []
                                                        [FunctionDecl "f" [Argument "x" "int", Argument "y" "int"] [Return (BinaryExp Plus (VariableExp "x") (VariableExp "y"))]]
                                                        (Just (FunctionCall "f" [Constant (Number 1), Constant (Number 2)]))))
        ~?= "def f(x: int, y: int):\n\treturn x + y\n\nf(1, 2)"

    , "write program with struct"
        ~: (execWriter $ writePython (Program Nothing
                                                        [StructDecl "City" [Argument "lat" "double", Argument "lon" "double"]]
                                                        [FunctionDecl "f" [Argument "x" "int", Argument "y" "int"] [Return (StructExpr (Struct "City" [VariableExp "x", VariableExp "y"]))]]
                                                        (Just (FunctionCall "f" [Constant (Decimal 68.04), Constant (Decimal 16.08)]))))
        ~?= "class City:\n\tdef __init__(self, lat, lon):\n\t\tself.lat = lat\n\t\tself.lon = lon\n\ndef f(x: int, y: int):\n\treturn City(x, y)\n\nf(68.04, 16.08)"

    , "write full program"
        ~: (execWriter $ writePython (Program (Just (ProgramDescription "Two decimal numbers x and y." "A new struct with x and y as latitude and longiture values."))
                                                        [StructDecl "City" [Argument "lat" "double", Argument "lon" "double"]]
                                                        [FunctionDecl "f" [Argument "x" "int", Argument "y" "int"] [Return (StructExpr (Struct "City" [VariableExp "x", VariableExp "y"]))]]
                                                        (Just (FunctionCall "f" [Constant (Decimal 68.04), Constant (Decimal 16.08)]))))
        ~?= "# Input: Two decimal numbers x and y.\n# Output: A new struct with x and y as latitude and longiture values.\n\nclass City:\n\tdef __init__(self, lat, lon):\n\t\tself.lat = lat\n\t\tself.lon = lon\n\ndef f(x: int, y: int):\n\treturn City(x, y)\n\nf(68.04, 16.08)"
    ]


testStructDecl :: Test
testStructDecl = TestList
    [ "write empty struct"
        ~: (execWriter $ writeClass (StructDecl "Person" []))
        ~?= "class Person:\n\tdef __init__(self):\n\t\tpass"

    , "write small struct"
        ~: (execWriter $ writeClass (StructDecl "Person" [Argument "age" "int"]))
        ~?= "class Person:\n\tdef __init__(self, age):\n\t\tself.age = age\n"

    , "write large struct"
        ~: (execWriter $ writeClass (StructDecl "Person" [Argument "name" "str", Argument "age" "int", Argument "city" "str", Argument "friends" "list", Argument "children" "int"]))
        ~?= "class Person:\n\tdef __init__(self, name, age, city, friends, children):\n\t\tself.name = name\n\t\tself.age = age\n\t\tself.city = city\n\t\tself.friends = friends\n\t\tself.children = children\n"
    ]

testStatements :: Test
testStatements = TestList
    [ "write assignment with variable target"
        ~: (execWriter $ writeStmt (Assignment (VariableTarget "var'") (ExpressionValue (Constant (Decimal 20.24)))) 0)
        ~?= "var' = 20.24\n"

    , "write assignment with list index target"
        ~: (execWriter $ writeStmt (Assignment (ListIndexTarget "people" [Constant (Number 20)]) (StructValue (Struct "Person" [Constant (Text "Giuliano"), Constant (Number 70)]))) 0)
        ~?= "people[20] = Person(\"Giuliano\", 70)\n"

    , "write assignment with struct field target"
        ~: (execWriter $ writeStmt (Assignment (StructFieldTarget (StructField (VariableExp "sicilia") (VariableExp "breakfast"))) (ExpressionValue (Constant (Text "cornetto")))) 0)
        ~?= "sicilia.breakfast = \"cornetto\"\n"

    , "write while loop"
        ~: (execWriter $ writeStmt (Loop (BinaryExp LessThan (VariableExp "x") (Constant (Number 10)))
                                            [Assignment (VariableTarget "x") (ExpressionValue (BinaryExp Plus (VariableExp "x") (Constant (Number 1))))]) 0)
        ~?= "while x < 10:\n\tx = x + 1\n"

    , "write if without else"
        ~: (execWriter $ writeStmt (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                                            [Return (Constant (Boolean True))]
                                            Nothing) 0)
        ~?= "if x == 10:\n\treturn True\n"

    , "write if with else if"
        ~: (execWriter $ writeStmt (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                                            [Return (Constant (Boolean True))]
                                            (Just (ElseIf (Constant (Boolean True)) [Return (Constant (Boolean False))] Nothing))) 0)
        ~?= "if x == 10:\n\treturn True\nelif True:\n\treturn False\n"

    , "write if with else"
        ~: (execWriter $ writeStmt (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                                            [Return (Constant (Boolean True))]
                                            (Just (Else [Return (Constant (Boolean False))]))) 0)
        ~?= "if x == 10:\n\treturn True\nelse:\n\treturn False\n"

    , "write foreach loop"
        ~: (execWriter $ writeStmt (ForEach "team" (VariableExp "teams")
                                            [CallStmt (FunctionCall "print" [VariableExp "team"])]) 0)
        ~?= "for team in teams:\n\tprint(team)\n"

    , "write for loop"
        ~: (execWriter $ writeStmt (For "index" (Constant (Number 0)) (Constant (Number 10))
                                            [CallStmt (FunctionCall "print" [VariableExp "index"])]) 0)
        ~?= "for index in range(0, 10):\n\tprint(index)\n"

    , "write function call without arguments"
        ~: (execWriter $ writeStmt (CallStmt (FunctionCall "f" [])) 0)
        ~?= "f()\n"

    , "write function call with one argument"
        ~: (execWriter $ writeStmt (CallStmt (FunctionCall "f" [Constant (Number 1)])) 0)
        ~?= "f(1)\n"

    , "write function call with multiple arguments"
        ~: (execWriter $ writeStmt (CallStmt (FunctionCall "f" [Constant (Number 1), Constant (Text "Palermo"), Constant (Decimal 45.72), Constant (List [])])) 0)
        ~?= "f(1, \"Palermo\", 45.72, [])\n"

    , "write returning variable"
        ~: (execWriter $ writeStmt (Return (VariableExp "result")) 0)
        ~?= "return result\n"

    , "write returning list"
        ~: (execWriter $ writeStmt (Return (Constant (List [Constant (Number 1), Constant (Number 2)]))) 0)
        ~?= "return [1, 2]\n"

    , "write returning struct"
        ~: (execWriter $ writeStmt (Return (StructExpr (Struct "Person" [VariableExp "x", VariableExp "y"]))) 0)
        ~?= "return Person(x, y)\n"

    , "write hash stmt"
        ~: (execWriter $ writeStmt (HashStmt (Return (VariableExp "x"))) 0)
        ~?= "return x\n"

    , "write annotation stmt"
        ~: (execWriter $ writeStmt (AnnotationStmt "p <- choosePivot(liste)" [Assignment (VariableTarget "p") (ExpressionValue (ListIndex "liste" [Constant (Number 0)]))]) 0)
        ~?= "p = liste[0]\n"

    , "write break stmt"
        ~: (execWriter $ writeStmt Break 0)
        ~?= "break\n"

    , "write continue stmt"
        ~: (execWriter $ writeStmt Continue 0)
        ~?= "continue\n"
    ]


testExpressions :: Test
testExpressions = TestList
    [ "write function call without arguments"
        ~: (execWriter $ writeExpr (CallExp (FunctionCall "f" [])))
        ~?= "f()"

    , "write function call with one argument"
        ~: (execWriter $ writeExpr (CallExp (FunctionCall "f" [Constant (Number 1)])))
        ~?= "f(1)"

    , "write function call with multiple arguments"
        ~: (execWriter $ writeExpr (CallExp (FunctionCall "f" [Constant (Number 1), Constant (Text "Palermo"), Constant (Decimal 45.72), Constant (List [])])))
        ~?= "f(1, \"Palermo\", 45.72, [])"

    , "write list index access"
        ~: (execWriter $ writeExpr (ListIndex "list" [Constant (Number 1)]))
        ~?= "list[1]"

    , "write nested list index access"
        ~: (execWriter $ writeExpr (ListIndex "list" [Constant (Number 1), Constant (Number 2), Constant (Number 3)]))
        ~?= "list[1][2][3]"

    , "write structfield access"
        ~: (execWriter $ writeExpr (StructFieldExp (StructField (VariableExp "struct") (VariableExp "field"))))
        ~?= "struct.field"

    , "write nested structfield access"
        ~: (execWriter $ writeExpr (StructFieldExp (StructField (VariableExp "struct") (StructFieldExp (StructField (VariableExp "field") (VariableExp "nested"))))))
        ~?= "struct.field.nested"

    , "write struct expression without arguments"
        ~: (execWriter $ writeExpr (StructExpr (Struct "Person" [])))
        ~?= "Person()"

    , "write struct expression with one argument"
        ~: (execWriter $ writeExpr (StructExpr (Struct "Person" [Constant (Number 1)])))
        ~?= "Person(1)"

    , "write struct expression with multiple arguments"
        ~: (execWriter $ writeExpr (StructExpr (Struct "Person" [Constant (Number 1), Constant (Text "Sicilia"), StructExpr (Struct "Country" [Constant (Text "Italia")])])))
        ~?= "Person(1, \"Sicilia\", Country(\"Italia\"))"

    , "write simple variable name"
        ~: (execWriter $ writeExpr (VariableExp "variable"))
        ~?= "variable"

    , "write variable name with symbol"
        ~: (execWriter $ writeExpr (VariableExp "variable'"))
        ~?= "variable'"

    , "write negated function call"
        ~: (execWriter $ writeExpr (Not (CallExp (FunctionCall "f" [Constant (Number 1)]))))
        ~?= "not f(1)"

    , "write negated list index access"
        ~: (execWriter $ writeExpr (Not (ListIndex "list" [Constant (Number 1)])))
        ~?= "not list[1]"

    , "write negated nested list index access"
        ~: (execWriter $ writeExpr (Not (ListIndex "list" [Constant (Number 1), Constant (Number 2), Constant (Number 3)])))
        ~?= "not list[1][2][3]"

    , "write negated struct expression"
        ~: (execWriter $ writeExpr (Not (StructExpr (Struct "Person" [Constant (Number 1)]))))
        ~?= "not Person(1)"

    , "write negated variable"
        ~: (execWriter $ writeExpr (Not (VariableExp "variable")))
        ~?= "not variable"

    , "write text constant"
        ~: (execWriter $ writeExpr (Constant (Text "var")))
        ~?= "\"var\""

    , "write number constant"
        ~: (execWriter $ writeExpr (Constant (Number 1905)))
        ~?= "1905"

    , "write set constant"
        ~: (execWriter $ writeExpr (Constant (HashSet (Set.fromList [Constant (Number 2), Constant (Number 3), Constant (Number 2)]))))
        ~?= "{2, 3}"
    ]

testProgramDescription :: Test
testProgramDescription = TestList
    [ "write empty program description"
        ~: (execWriter $ writeProgramDescription (Just (ProgramDescription "" "")))
        ~?= "# Input: \n# Output: \n\n"

    , "write random program description"
        ~: (execWriter $ writeProgramDescription (Just (ProgramDescription "description of input" "description of output")))
        ~?= "# Input: description of input\n# Output: description of output\n\n"
    ]

testValues :: Test
testValues = TestList
    [ "write small number"
        ~: (execWriter $ writeValue (Number 5))
        ~?= "5"

    , "write big number"
        ~: (execWriter $ writeValue (Number 42174871982315980219241))
        ~?= "42174871982315980219241"

    , "write nil"
        ~: (execWriter $ writeValue Nil)
        ~?= "None"

    , "write positive boolean"
        ~: (execWriter $ writeValue (Boolean True))
        ~?= "True"

    , "write negative boolean"
        ~: (execWriter $ writeValue (Boolean False))
        ~?= "False"

    , "write decimal"
        ~: (execWriter $ writeValue (Decimal 3.14))
        ~?= "3.14"

    , "write empty text"
        ~: (execWriter $ writeValue (Text ""))
        ~?= "\"\""

    , "write regular text"
        ~: (execWriter $ writeValue (Text "informatics"))
        ~?= "\"informatics\""

    , "write text with spaces"
        ~: (execWriter $ writeValue (Text "I love informatics"))
        ~?= "\"I love informatics\""

    , "write empty list"
        ~: (execWriter $ writeValue (List []))
        ~?= "[]"

    , "write list with numbers"
        ~: (execWriter $ writeValue (List [Constant (Number 1), Constant (Number 2), Constant (Number 3)]))
        ~?= "[1, 2, 3]"

    , "write nested list"
        ~: (execWriter $ writeValue (List [Constant (Number 1), Constant (List [Constant (List [Constant (Number 2)]), Constant (List [Constant (Number 3)])])]))
        ~?= "[1, [[2], [3]]]"

    , "write empty hashset"
        ~: (execWriter $ writeValue (HashSet (Set.fromList [])))
        ~?= "set()"

    , "write hashset"
        ~: (execWriter $ writeValue (HashSet (Set.fromList [Constant (Number 1), Constant (Number 2), Constant (Number 1), Constant (Number 2), Constant (Number 1), Constant (Number 2)])))
        ~?= "{1, 2}"

    , "write empty hashmap"
        ~: (execWriter $ writeValue (HashMap (Map.fromList [])))
        ~?= "{}"

    , "write hashmap with string keys"
        ~: (execWriter $ writeValue (HashMap (Map.fromList [(Constant (Text "key"), Constant (Text "value")), (Constant (Text "goals"), Constant (Number 123))])))
        ~?= "{\"goals\": 123, \"key\": \"value\"}"

    , "write hashmap with different type keys"
        ~: (execWriter $ writeValue (HashMap (Map.fromList [(Constant (Number 1), Constant (Text "value")), (Constant (Decimal 2.5), Constant (Number 123))])))
        ~?= "{1: \"value\", 2.5: 123}"
    ]
