module Parsers.Gourmet (testGourmetParser) where

import Syntax
import Gourmet.GourmetParser
    ( parseValue
    , parseProgramDescription
    , parseExpr
    , parseStmt
    , parseStructDecl
    , parseGourmet
    )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.HUnit
import Text.Parsec


testGourmetParser :: Test
testGourmetParser = TestList
    [ testProgramDescriptions
    , testValues
    , testExpressions
    , testStatements
    , testStructDecl
    , testProgram
    ]

testProgram :: Test
testProgram = TestList
    [ "parse empty program"
        ~: parse parseGourmet "" ""
        ~?= Right (Program Nothing [] [] Nothing)

    , "parse program with just description"
        ~: parse parseGourmet "" "?There is no program.? !Thus nothing is returned.!"
        ~?= Right (Program (Just (ProgramDescription "There is no program." "Thus nothing is returned.")) [] [] Nothing)

    , "parse program with just struct"
        ~: parse parseGourmet "" "struct Person { age int }"
        ~?= Right (Program Nothing [StructDecl "Person" [Argument "age" "int"]] [] Nothing)

    , "parse program with just function declaration"
        ~: parse parseGourmet "" "func f() { return 1 }"
        ~?= Right (Program Nothing [] [FunctionDecl "f" [] [Return (Constant (Number 1))]] Nothing)

    , "parse simple program"
        ~: parse parseGourmet "" "func f(x int, y int) { return x + y } f(1, 2)"
        ~?= Right (Program Nothing
                           []
                           [FunctionDecl "f" [Argument "x" "int", Argument "y" "int"] [Return (BinaryExp Plus (VariableExp "x") (VariableExp "y"))]]
                           (Just (FunctionCall "f" [Constant (Number 1), Constant (Number 2)])))

    , "parse program with struct"
        ~: parse parseGourmet "" "struct City { lat double, lon double } func f(x int, y int) { return struct City(x, y) } f(68.04, 16.08)"
        ~?= Right (Program Nothing
                           [StructDecl "City" [Argument "lat" "double", Argument "lon" "double"]]
                           [FunctionDecl "f" [Argument "x" "int", Argument "y" "int"] [Return (StructExpr (Struct "City" [VariableExp "x", VariableExp "y"]))]]
                           (Just (FunctionCall "f" [Constant (Decimal 68.04), Constant (Decimal 16.08)])))

    , "parse full program"
        ~: parse parseGourmet "" "?Two decimal numbers x and y.? !A new struct with x and y as latitude and longiture values.! struct City { lat double, lon double } func f(x int, y int) { return struct City(x, y) } f(68.04, 16.08)"
        ~?= Right (Program (Just (ProgramDescription "Two decimal numbers x and y." "A new struct with x and y as latitude and longiture values."))
                           [StructDecl "City" [Argument "lat" "double", Argument "lon" "double"]]
                           [FunctionDecl "f" [Argument "x" "int", Argument "y" "int"] [Return (StructExpr (Struct "City" [VariableExp "x", VariableExp "y"]))]]
                           (Just (FunctionCall "f" [Constant (Decimal 68.04), Constant (Decimal 16.08)])))
    ]

testStructDecl :: Test
testStructDecl = TestList
    [ "parse empty struct"
        ~: parse parseStructDecl "" "struct Person { }"
        ~?= Right (StructDecl "Person" [])

    , "parse small struct"
        ~: parse parseStructDecl "" "struct Person { age int }"
        ~?= Right (StructDecl "Person" [Argument "age" "int"])

    , "parse large struct"
        ~: parse parseStructDecl "" "struct Person { name str, age int, city str, friends list, children int}"
        ~?= Right (StructDecl "Person" [Argument "name" "str", Argument "age" "int", Argument "city" "str", Argument "friends" "list", Argument "children" "int"])
    ]

testStatements :: Test
testStatements = TestList
    [ "parse assignment with variable target"
        ~: parse parseStmt "" "var' := 20.24"
        ~?= Right (Assignment (VariableTarget "var'") (ExpressionValue (Constant (Decimal 20.24))))

    , "parse assignment with list index target"
        ~: parse parseStmt "" "people[20] := struct Person(\"Giuliano\", 70)"
        ~?= Right (Assignment (ListIndexTarget "people" [Constant (Number 20)]) (StructValue (Struct "Person" [Constant (Text "Giuliano"), Constant (Number 70)])))

    , "parse assignment with struct field target"
        ~: parse parseStmt "" "sicilia.breakfast := \"cornetto\" "
        ~?= Right (Assignment (StructFieldTarget (StructField (VariableExp "sicilia") (VariableExp "breakfast"))) (ExpressionValue (Constant (Text "cornetto"))))

    , "parse while loop"
        ~: parse parseStmt "" "while x < 10 { x := x + 1 }"
        ~?= Right (Loop (BinaryExp LessThan (VariableExp "x") (Constant (Number 10)))
                        [Assignment (VariableTarget "x") (ExpressionValue (BinaryExp Plus (VariableExp "x") (Constant (Number 1))))])

    , "parse if without else"
        ~: parse parseStmt "" "if x == 10 { return true }"
        ~?= Right (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                      [Return (Constant (Boolean True))]
                      Nothing)

    , "parse if with else if"
        ~: parse parseStmt "" "if x == 10 { return true } else if true { return false }"
        ~?= Right (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                      [Return (Constant (Boolean True))]
                      (Just (ElseIf (Constant (Boolean True)) [Return (Constant (Boolean False))] Nothing)))

    , "parse if with else"
        ~: parse parseStmt "" "if x == 10 { return true } else { return false }"
        ~?= Right (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                      [Return (Constant (Boolean True))]
                      (Just (Else [Return (Constant (Boolean False))])))

    , "parse foreach loop"
        ~: parse parseStmt "" "for team := teams { print(team) }"
        ~?= Right (ForEach "team" (VariableExp "teams")
                                  [CallStmt (FunctionCall "print" [VariableExp "team"])])

    , "parse for loop"
        ~: parse parseStmt "" "for index := 0, 10 { print(index) }"
        ~?= Right (For "index" (Constant (Number 0)) (Constant (Number 10))
                               [CallStmt (FunctionCall "print" [VariableExp "index"])])

    , "parse function call without arguments"
        ~: parse parseStmt "" "f()"
        ~?= Right (CallStmt (FunctionCall "f" []))

    , "parse function call with one argument"
        ~: parse parseStmt "" "f(1)"
        ~?= Right (CallStmt (FunctionCall "f" [Constant (Number 1)]))

    , "parse function call with multiple arguments"
        ~: parse parseStmt "" "f(1, \"Palermo\", 45.72, [])"
        ~?= Right (CallStmt (FunctionCall "f" [Constant (Number 1), Constant (Text "Palermo"), Constant (Decimal 45.72), Constant (List [])]))

    , "parse returning variable"
        ~: parse parseStmt "" "return result"
        ~?= Right (Return (VariableExp "result"))

    , "parse returning list"
        ~: parse parseStmt "" "return [1, 2]"
        ~?= Right (Return (Constant (List [Constant (Number 1), Constant (Number 2)])))

    , "parse returning struct"
        ~: parse parseStmt "" "return struct Person(x, y)"
        ~?= Right (Return (StructExpr (Struct "Person" [VariableExp "x", VariableExp "y"])))

    , "parse hash stmt"
        ~: parse parseStmt "" "# return x"
        ~?= Right (HashStmt (Return (VariableExp "x")))

    , "parse annotation stmt"
        ~: parse parseStmt "" "@{p <- choosePivot(liste)}{ p := liste[0] }"
        ~?= Right (AnnotationStmt "p <- choosePivot(liste)" [Assignment (VariableTarget "p") (ExpressionValue (ListIndex "liste" [Constant (Number 0)]))])

    , "parse stmt"
        ~: parse parseStmt "" "break"
        ~?= Right (Break)

    , "parse stmt"
        ~: parse parseStmt "" "continue"
        ~?= Right (Continue)
    ]

testExpressions :: Test
testExpressions = TestList
    [ "parse function call without arguments"
        ~: parse parseExpr "" "f()"
        ~?= Right (CallExp (FunctionCall "f" []))

    , "parse function call with one argument"
        ~: parse parseExpr "" "f(1)"
        ~?= Right (CallExp (FunctionCall "f" [Constant (Number 1)]))

    , "parse function call with multiple arguments"
        ~: parse parseExpr "" "f(1, \"Palermo\", 45.72, [])"
        ~?= Right (CallExp (FunctionCall "f" [Constant (Number 1), Constant (Text "Palermo"), Constant (Decimal 45.72), Constant (List [])]))

    , "parse list index access"
        ~: parse parseExpr "" "list[1]"
        ~?= Right (ListIndex "list" [Constant (Number 1)])

    , "parse nested list index access"
        ~: parse parseExpr "" "list[1][2][3]"
        ~?= Right (ListIndex "list" [Constant (Number 1), Constant (Number 2), Constant (Number 3)])

    -- , "parse structfield access"
    --     ~: parse parseExpr "" "struct.field"
    --     ~?= Right (StructFieldExp (StructField (VariableExp "struct") (VariableExp "field")))

    -- , "parse nested structfield access"
    --     ~: parse parseExpr "" "struct.field.nested"
    --     ~?= Right (StructFieldExp (StructField (VariableExp "struct") (StructFieldExp (StructField (VariableExp "field") (VariableExp "nested")))))

    , "parse struct expression without arguments"
        ~: parse parseExpr "" "struct Person()"
        ~?= Right (StructExpr (Struct "Person" []))

    , "parse struct expression with one argument"
        ~: parse parseExpr "" "struct Person(1)"
        ~?= Right (StructExpr (Struct "Person" [Constant (Number 1)]))

    , "parse struct expression with multiple arguments"
        ~: parse parseExpr "" "struct Person(1, \"Sicilia\", struct Country(\"Italia\"))"
        ~?= Right (StructExpr (Struct "Person" [Constant (Number 1), Constant (Text "Sicilia"), StructExpr (Struct "Country" [Constant (Text "Italia")])]))

    , "parse simple variable name"
        ~: parse parseExpr "" "variable"
        ~?= Right (VariableExp "variable")

    , "parse variable name with symbol"
        ~: parse parseExpr "" "variable'"
        ~?= Right (VariableExp "variable'")

    , "parse negated function call"
        ~: parse parseExpr "" "not f(1)"
        ~?= Right (Not (CallExp (FunctionCall "f" [Constant (Number 1)])))

    , "parse negated list index access"
        ~: parse parseExpr "" "not list[1]"
        ~?= Right (Not (ListIndex "list" [Constant (Number 1)]))

    , "parse negated nested list index access"
        ~: parse parseExpr "" "not list[1][2][3]"
        ~?= Right (Not (ListIndex "list" [Constant (Number 1), Constant (Number 2), Constant (Number 3)]))

    , "parse negated struct expression"
        ~: parse parseExpr "" "not struct Person(1)"
        ~?= Right (Not (StructExpr (Struct "Person" [Constant (Number 1)])))

    , "parse negated variable"
        ~: parse parseExpr "" "not variable"
        ~?= Right (Not (VariableExp "variable"))

    , "parse text constant"
        ~: parse parseExpr "" "\"var\""
        ~?= Right (Constant (Text "var"))

    , "parse number constant"
        ~: parse parseExpr "" "1905"
        ~?= Right (Constant (Number 1905))

    , "parse set constant"
        ~: parse parseExpr "" "set{2, 3, 2}"
        ~?= Right (Constant (HashSet (Set.fromList [Constant (Number 2), Constant (Number 3), Constant (Number 2)])))
    ]

testProgramDescriptions :: Test
testProgramDescriptions = TestList
    [ "parse empty program description"
        ~: parse parseProgramDescription "" "??!!"
        ~?= Right (ProgramDescription "" "")

    , "parse random program description"
        ~: parse parseProgramDescription "" "?description of input? !description of output!"
        ~?= Right (ProgramDescription "description of input" "description of output")
    ]

testValues :: Test
testValues = TestList
    [ "parse small number"
        ~: parse parseValue "" "5"
        ~?= Right (Number 5)

    , "parse big number"
        ~: parse parseValue "" "42174871982315980219241"
        ~?= Right (Number 42174871982315980219241)

    , "parse nil"
        ~: parse parseValue "" "nil"
        ~?= Right Nil

    , "parse positive boolean"
        ~: parse parseValue "" "true"
        ~?= Right (Boolean True)

    , "parse negative boolean"
        ~: parse parseValue "" "false"
        ~?= Right (Boolean False)

    , "parse decimal"
        ~: parse parseValue "" "3.14"
        ~?= Right (Decimal 3.14)

    , "parse empty text"
        ~: parse parseValue "" "\"\""
        ~?= Right (Text "")

    , "parse regular text"
        ~: parse parseValue "" "\"informatics\""
        ~?= Right (Text "informatics")

    , "parse text with spaces"
        ~: parse parseValue "" "\"I love informatics\""
        ~?= Right (Text "I love informatics")

    , "parse empty list"
        ~: parse parseValue "" "[]"
        ~?= Right (List [])

    , "parse list with numbers"
        ~: parse parseValue "" "[1, 2, 3]"
        ~?= Right (List [Constant (Number 1), Constant (Number 2), Constant (Number 3)])

    , "parse nested list"
        ~: parse parseValue "" "[1, [[2], [3]]]"
        ~?= Right (List [Constant (Number 1), Constant (List [Constant (List [Constant (Number 2)]), Constant (List [Constant (Number 3)])])])

    , "parse empty hashset"
        ~: parse parseValue "" "set {}"
        ~?= Right (HashSet (Set.fromList []))

    , "parse hashset"
        ~: parse parseValue "" "set {1, 2, 2, 1, 2}"
        ~?= Right (HashSet (Set.fromList [Constant (Number 1), Constant (Number 2), Constant (Number 2), Constant (Number 1), Constant (Number 2)]))

    , "parse empty hashmap"
        ~: parse parseValue "" "map {}"
        ~?= Right (HashMap (Map.fromList []))

    , "parse hashmap with string keys"
        ~: parse parseValue "" "map {\"key\": \"value\", \"goals\": 123}"
        ~?= Right (HashMap (Map.fromList [(Constant (Text "key"), Constant (Text "value")), (Constant (Text "goals"), Constant (Number 123))]))

    , "parse hashmap with different type keys"
        ~: parse parseValue "" "map{1: \"value\", 2.5: 123}"
        ~?= Right (HashMap (Map.fromList [(Constant (Number 1), Constant (Text "value")), (Constant (Decimal 2.5), Constant (Number 123))]))
    ]
