module Writers.Gourmet (testGourmetWriter) where

import Syntax
import Gourmet.GourmetWriter
    ( writeValue
    , writeProgramDescription
    , writeExpr
    , writeStmt
    , writeStructDecl
    , writeGourmet
    )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Writer
import Test.HUnit


testGourmetWriter :: Test
testGourmetWriter = TestList
    [ testProgramDescriptions
    , testValues
    , testExpressions
    , testStatements
    , testStructDecl
    , testProgram
    ]

testProgram :: Test
testProgram = TestList
    [ "write empty program"
        ~: let res = execWriter $ writeGourmet (Program Nothing [] [] Nothing)
           in res
        ~?= ""

    , "write program with just description"
        ~: let res = execWriter $ writeGourmet (Program (Just (ProgramDescription "There is no program." "Thus nothing is returned.")) [] [] Nothing)
           in res
        ~?= "? There is no program. ?\n! Thus nothing is returned. !\n\n"

    , "write program with just struct"
        ~: let res = execWriter $ writeGourmet (Program Nothing [StructDecl "Person" [Argument "age" "int"]] [] Nothing)
           in res
        ~?= "struct Person {\n\tage int\n}\n\n"

    , "write program with just function declaration"
        ~: let res = execWriter $ writeGourmet (Program Nothing [] [Function "f" [] [Return (Constant (Number 1))]] Nothing)
           in res
        ~?= "func f() {\n\treturn 1\n}\n\n"

    , "write simple program"
        ~: let res = execWriter $ writeGourmet (Program Nothing
                                                        []
                                                        [Function "f" [Argument "x" "int", Argument "y" "int"] [Return (BinaryExp Plus (VariableExp "x") (VariableExp "y"))]]
                                                        (Just (FunctionCall "f" [Constant (Number 1), Constant (Number 2)])))
           in res
        ~?= "func f(x int, y int) {\n\treturn x + y\n}\n\nf(1, 2)"

    , "write program with struct"
        ~: let res = execWriter $ writeGourmet (Program Nothing
                                                        [StructDecl "City" [Argument "lat" "double", Argument "lon" "double"]]
                                                        [Function "f" [Argument "x" "int", Argument "y" "int"] [Return (StructExpr (Struct "City" [VariableExp "x", VariableExp "y"]))]]
                                                        (Just (FunctionCall "f" [Constant (Decimal 68.04), Constant (Decimal 16.08)])))
           in res
        ~?= "struct City {\n\tlat double,\n\tlon double\n}\n\nfunc f(x int, y int) {\n\treturn struct City(x, y)\n}\n\nf(68.04, 16.08)"

    , "write full program"
        ~: let res = execWriter $ writeGourmet (Program (Just (ProgramDescription "Two decimal numbers x and y." "A new struct with x and y as latitude and longiture values."))
                                                        [StructDecl "City" [Argument "lat" "double", Argument "lon" "double"]]
                                                        [Function "f" [Argument "x" "int", Argument "y" "int"] [Return (StructExpr (Struct "City" [VariableExp "x", VariableExp "y"]))]]
                                                        (Just (FunctionCall "f" [Constant (Decimal 68.04), Constant (Decimal 16.08)])))
           in res
        ~?= "? Two decimal numbers x and y. ?\n! A new struct with x and y as latitude and longiture values. !\n\nstruct City {\n\tlat double,\n\tlon double\n}\n\nfunc f(x int, y int) {\n\treturn struct City(x, y)\n}\n\nf(68.04, 16.08)"
    ]


testStructDecl :: Test
testStructDecl = TestList
    [ "write empty struct"
        ~: let res = execWriter $ writeStructDecl (StructDecl "Person" [])
           in res
        ~?= "struct Person {}"

    , "write small struct"
        ~: let res = execWriter $ writeStructDecl (StructDecl "Person" [Argument "age" "int"])
           in res
        ~?= "struct Person {\n\tage int\n}"

    , "write large struct"
        ~: let res = execWriter $ writeStructDecl (StructDecl "Person" [Argument "name" "str", Argument "age" "int", Argument "city" "str", Argument "friends" "list", Argument "children" "int"])
           in res
        ~?= "struct Person {\n\tname str,\n\tage int,\n\tcity str,\n\tfriends list,\n\tchildren int\n}"
    ]

testStatements :: Test
testStatements = TestList
    [ "write assignment with variable target"
        ~: let res = execWriter $ writeStmt (Assignment (VariableTarget "var'") (ExpressionValue (Constant (Decimal 20.24)))) 0
           in res
        ~?= "var' := 20.24"

    , "write assignment with list index target"
        ~: let res = execWriter $ writeStmt (Assignment (ListIndexTarget "people" [Constant (Number 20)]) (StructValue (Struct "Person" [Constant (Text "Giuliano"), Constant (Number 70)]))) 0
           in res
        ~?= "people[20] := struct Person(\"Giuliano\", 70)"

    , "write assignment with struct field target"
        ~: let res = execWriter $ writeStmt (Assignment (StructFieldTarget (StructField (VariableExp "sicilia") (VariableExp "breakfast"))) (ExpressionValue (Constant (Text "cornetto")))) 0
           in res
        ~?= "sicilia.breakfast := \"cornetto\""

    , "write while loop"
        ~: let res = execWriter $ writeStmt (Loop (BinaryExp LessThan (VariableExp "x") (Constant (Number 10)))
                                            [Assignment (VariableTarget "x") (ExpressionValue (BinaryExp Plus (VariableExp "x") (Constant (Number 1))))]) 0
           in res
        ~?= "while x < 10 {\n\tx := x + 1\n}"

    , "write if without else"
        ~: let res = execWriter $ writeStmt (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                                            [Return (Constant (Boolean True))]
                                            Nothing) 0
           in res
        ~?= "if x == 10 {\n\treturn true\n}"

    , "write if with else if"
        ~: let res = execWriter $ writeStmt (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                                            [Return (Constant (Boolean True))]
                                            (Just (ElseIf (Constant (Boolean True)) [Return (Constant (Boolean False))] Nothing))) 0
           in res
        ~?= "if x == 10 {\n\treturn true\n} else if true {\n\treturn false\n}"

    , "write if with else"
        ~: let res = execWriter $ writeStmt (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                                            [Return (Constant (Boolean True))]
                                            (Just (Else [Return (Constant (Boolean False))]))) 0
           in res
        ~?= "if x == 10 {\n\treturn true\n} else {\n\treturn false\n}"

    , "write foreach loop"
        ~: let res = execWriter $ writeStmt (ForEach "team" (VariableExp "teams")
                                            [CallStmt (FunctionCall "print" [VariableExp "team"])]) 0
           in res
        ~?= "for team := teams {\n\tprint(team)\n}"

    , "write for loop"
        ~: let res = execWriter $ writeStmt (For "index" (Constant (Number 0)) (Constant (Number 10))
                                            [CallStmt (FunctionCall "print" [VariableExp "index"])]) 0
           in res
        ~?= "for index := 0, 10 {\n\tprint(index)\n}"

    , "write function call without arguments"
        ~: let res = execWriter $ writeStmt (CallStmt (FunctionCall "f" [])) 0
           in res
        ~?= "f()"

    , "write function call with one argument"
        ~: let res = execWriter $ writeStmt (CallStmt (FunctionCall "f" [Constant (Number 1)])) 0
           in res
        ~?= "f(1)"

    , "write function call with multiple arguments"
        ~: let res = execWriter $ writeStmt (CallStmt (FunctionCall "f" [Constant (Number 1), Constant (Text "Palermo"), Constant (Decimal 45.72), Constant (List [])])) 0
           in res
        ~?= "f(1, \"Palermo\", 45.72, [])"

    , "write returning variable"
        ~: let res = execWriter $ writeStmt (Return (VariableExp "result")) 0
           in res
        ~?= "return result"

    , "write returning list"
        ~: let res = execWriter $ writeStmt (Return (Constant (List [Constant (Number 1), Constant (Number 2)]))) 0
           in res
        ~?= "return [1, 2]"

    , "write returning struct"
        ~: let res = execWriter $ writeStmt (Return (StructExpr (Struct "Person" [VariableExp "x", VariableExp "y"]))) 0
           in res
        ~?= "return struct Person(x, y)"

    , "write empty hash stmt"
        ~: let res = execWriter $ writeStmt (HashStmt (Return (VariableExp "x"))) 0
           in res
        ~?= "# return x"

    , "write annotation stmt"
        ~: let res = execWriter $ writeStmt (AnnotationStmt "p <- choosePivot(liste)" [Assignment (VariableTarget "p") (ExpressionValue (ListIndex "liste" [Constant (Number 0)]))]) 0
           in res
        ~?= "@{p <- choosePivot(liste)}{\n\tp := liste[0]\n}"

    , "write stmt"
        ~: let res = execWriter $ writeStmt Break 0
           in res
        ~?= "break"

    , "write stmt"
        ~: let res = execWriter $ writeStmt Continue 0
           in res
        ~?= "continue"
    ]


testExpressions :: Test
testExpressions = TestList
    [ "write function call without arguments"
        ~: let res = execWriter $ writeExpr (CallExp (FunctionCall "f" []))
           in res
        ~?= "f()"

    , "write function call with one argument"
        ~: let res = execWriter $ writeExpr (CallExp (FunctionCall "f" [Constant (Number 1)]))
           in res
        ~?= "f(1)"

    , "write function call with multiple arguments"
        ~: let res = execWriter $ writeExpr (CallExp (FunctionCall "f" [Constant (Number 1), Constant (Text "Palermo"), Constant (Decimal 45.72), Constant (List [])]))
           in res
        ~?= "f(1, \"Palermo\", 45.72, [])"

    , "write list index access"
        ~: let res = execWriter $ writeExpr (ListIndex "list" [Constant (Number 1)])
           in res
        ~?= "list[1]"

    , "write nested list index access"
        ~: let res = execWriter $ writeExpr (ListIndex "list" [Constant (Number 1), Constant (Number 2), Constant (Number 3)])
           in res
        ~?= "list[1][2][3]"

    , "write structfield access"
        ~: let res = execWriter $ writeExpr (StructFieldExp (StructField (VariableExp "struct") (VariableExp "field")))
           in res
        ~?= "struct.field"

    , "write nested structfield access"
        ~: let res = execWriter $ writeExpr (StructFieldExp (StructField (VariableExp "struct") (StructFieldExp (StructField (VariableExp "field") (VariableExp "nested")))))
           in res
        ~?= "struct.field.nested"

    , "write struct expression without arguments"
        ~: let res = execWriter $ writeExpr (StructExpr (Struct "Person" []))
           in res
        ~?= "struct Person()"

    , "write struct expression with one argument"
        ~: let res = execWriter $ writeExpr (StructExpr (Struct "Person" [Constant (Number 1)]))
           in res
        ~?= "struct Person(1)"

    , "write struct expression with multiple arguments"
        ~: let res = execWriter $ writeExpr (StructExpr (Struct "Person" [Constant (Number 1), Constant (Text "Sicilia"), StructExpr (Struct "Country" [Constant (Text "Italia")])]))
           in res
        ~?= "struct Person(1, \"Sicilia\", struct Country(\"Italia\"))"

    , "write simple variable name"
        ~: let res = execWriter $ writeExpr (VariableExp "variable")
           in res
        ~?= "variable"

    , "write variable name with symbol"
        ~: let res = execWriter $ writeExpr (VariableExp "variable'")
           in res
        ~?= "variable'"

    , "write negated function call"
        ~: let res = execWriter $ writeExpr (Not (CallExp (FunctionCall "f" [Constant (Number 1)])))
           in res
        ~?= "not f(1)"

    , "write negated list index access"
        ~: let res = execWriter $ writeExpr (Not (ListIndex "list" [Constant (Number 1)]))
           in res
        ~?= "not list[1]"

    , "write negated nested list index access"
        ~: let res = execWriter $ writeExpr (Not (ListIndex "list" [Constant (Number 1), Constant (Number 2), Constant (Number 3)]))
           in res
        ~?= "not list[1][2][3]"

    , "write negated struct expression"
        ~: let res = execWriter $ writeExpr (Not (StructExpr (Struct "Person" [Constant (Number 1)])))
           in res
        ~?= "not struct Person(1)"

    , "write negated variable"
        ~: let res = execWriter $ writeExpr (Not (VariableExp "variable"))
           in res
        ~?= "not variable"

    , "write text constant"
        ~: let res = execWriter $ writeExpr (Constant (Text "var"))
           in res
        ~?= "\"var\""

    , "write number constant"
        ~: let res = execWriter $ writeExpr (Constant (Number 1905))
           in res
        ~?= "1905"

    , "write set constant"
        ~: let res = execWriter $ writeExpr (Constant (HashSet (Set.fromList [Constant (Number 2), Constant (Number 3), Constant (Number 2)])))
           in res
        ~?= "set{2, 3}"
    ]

testProgramDescriptions :: Test
testProgramDescriptions = TestList
    [ "write empty program description"
        ~: let res = execWriter $ writeProgramDescription (Just (ProgramDescription "" ""))
           in res
        ~?= "?  ?\n!  !\n\n"

    , "write random program description"
        ~: let res = execWriter $ writeProgramDescription (Just (ProgramDescription "description of input" "description of output"))
           in res
        ~?= "? description of input ?\n! description of output !\n\n"
    ]

testValues :: Test
testValues = TestList
    [ "write small number"
        ~: let res = execWriter $ writeValue (Number 5)
           in res
        ~?= "5"

    , "write big number"
        ~: let res = execWriter $ writeValue (Number 42174871982315980219241)
           in res
        ~?= "42174871982315980219241"

    , "write nil"
        ~: let res = execWriter $ writeValue Nil
           in res
        ~?= "nil"

    , "write positive boolean"
        ~: let res = execWriter $ writeValue (Boolean True)
           in res
        ~?= "true"

    , "write negative boolean"
        ~: let res = execWriter $ writeValue (Boolean False)
           in res
        ~?= "false"

    , "write decimal"
        ~: let res = execWriter $ writeValue (Decimal 3.14)
           in res
        ~?= "3.14"

    , "write empty text"
        ~: let res = execWriter $ writeValue (Text "")
           in res
        ~?= "\"\""

    , "write regular text"
        ~: let res = execWriter $ writeValue (Text "informatics")
           in res
        ~?= "\"informatics\""

    , "write text with spaces"
        ~: let res = execWriter $ writeValue (Text "I love informatics")
           in res
        ~?= "\"I love informatics\""

    , "write empty list"
        ~: let res = execWriter $ writeValue (List [])
           in res
        ~?= "[]"

    , "write list with numbers"
        ~: let res = execWriter $ writeValue (List [Constant (Number 1), Constant (Number 2), Constant (Number 3)])
           in res
        ~?= "[1, 2, 3]"

    , "write nested list"
        ~: let res = execWriter $ writeValue (List [Constant (Number 1), Constant (List [Constant (List [Constant (Number 2)]), Constant (List [Constant (Number 3)])])])
           in res
        ~?= "[1, [[2], [3]]]"

    , "write empty hashset"
        ~: let res = execWriter $ writeValue (HashSet (Set.fromList []))
           in res
        ~?= "set{}"

    , "write hashset"
        ~: let res = execWriter $ writeValue (HashSet (Set.fromList [Constant (Number 1), Constant (Number 2), Constant (Number 1), Constant (Number 2), Constant (Number 1), Constant (Number 2)]))
           in res
        ~?= "set{1, 2}"

    , "write empty hashmap"
        ~: let res = execWriter $ writeValue (HashMap (Map.fromList []))
           in res
        ~?= "map{}"

    , "write hashmap with string keys"
        ~: let res = execWriter $ writeValue (HashMap (Map.fromList [(Constant (Text "key"), Constant (Text "value")), (Constant (Text "goals"), Constant (Number 123))]))
           in res
        ~?= "map{\"goals\": 123, \"key\": \"value\"}"

    , "write hashmap with different type keys"
        ~: let res = execWriter $ writeValue (HashMap (Map.fromList [(Constant (Number 1), Constant (Text "value")), (Constant (Decimal 2.5), Constant (Number 123))]))
           in res
        ~?= "map{1: \"value\", 2.5: 123}"
    ]
