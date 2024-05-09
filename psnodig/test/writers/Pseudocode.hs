module Writers.Pseudocode (testPseudocodeWriter) where

import Syntax
import LaTeX.LatexWriter
    ( writeValue
    , writeProgramDescription
    , writeExpr
    , writeStmt
    , writeLatex
    )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Writer
import Control.Monad.Reader
import Test.HUnit


testPseudocodeWriter :: Test
testPseudocodeWriter = TestList
    [ testValues
    , testProgramDescriptions
    , testExpressions
    , testStatements
    , testProgram
    ]

testProgram :: Test
testProgram = TestList
    [ "write empty program"
        ~: let res = execWriter $ runReaderT (writeLatex (Program Nothing [] [] Nothing)) ([], [])
           in res
        ~?= ""

    , "write program with just description"
        ~: let res = execWriter $ runReaderT (writeLatex (Program (Just (ProgramDescription "There is no program." "Thus nothing is returned.")) [] [] Nothing)) ([], [])
           in res
        ~?= ""

    , "write program with just struct"
        ~: let res = execWriter $ runReaderT (writeLatex (Program Nothing [StructDecl "Person" [Argument "age" "int"]] [] Nothing)) ([], [])
           in res
        ~?= ""

    , "write program with just function declaration"
        ~: let res = execWriter $ runReaderT (writeLatex (Program Nothing [] [Function "f" [] [Return (Constant (Number 1))]] Nothing)) ([], [])
           in res
        ~?= "\\documentclass{standalone}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath,commath} \n\\usepackage[linesnumbered, ruled]{algorithm2e}\n\\SetKwProg{proc}{Procedure}{}{}\n\\DontPrintSemicolon\n\\renewcommand{\\thealgocf}{}\n\\begin{document}\n\n\\begin{algorithm}[H]\n\\proc{$\\f()$}{\n\t\\Return 1 \\;\n}\n\\caption{f}\n\\end{algorithm}\n\n\\end{document}"

    , "write simple program"
        ~: let res = execWriter $ runReaderT (writeLatex (Program Nothing
                                                        []
                                                        [Function "f" [Argument "x" "int", Argument "y" "int"] [Return (BinaryExp Plus (VariableExp "x") (VariableExp "y"))]]
                                                        (Just (FunctionCall "f" [Constant (Number 1), Constant (Number 2)])))) ([], [])
           in res
        ~?= "\\documentclass{standalone}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath,commath} \n\\usepackage[linesnumbered, ruled]{algorithm2e}\n\\SetKwProg{proc}{Procedure}{}{}\n\\DontPrintSemicolon\n\\renewcommand{\\thealgocf}{}\n\\begin{document}\n\n\\begin{algorithm}[H]\n\\proc{$\\f(x, y)$}{\n\t\\Return x + y \\;\n}\n\\caption{f}\n\\end{algorithm}\n\n\\end{document}"

    , "write program with struct"
        ~: let res = execWriter $ runReaderT (writeLatex (Program Nothing
                                                        [StructDecl "City" [Argument "lat" "double", Argument "lon" "double"]]
                                                        [Function "f" [Argument "x" "int", Argument "y" "int"] [Return (StructExpr (Struct "City" [VariableExp "x", VariableExp "y"]))]]
                                                        (Just (FunctionCall "f" [Constant (Decimal 68.04), Constant (Decimal 16.08)])))) ([], [])
           in res
        ~?= "\\documentclass{standalone}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath,commath} \n\\usepackage[linesnumbered, ruled]{algorithm2e}\n\\SetKwProg{proc}{Procedure}{}{}\n\\DontPrintSemicolon\n\\renewcommand{\\thealgocf}{}\n\\begin{document}\n\n\\begin{algorithm}[H]\n\\proc{$\\f(x, y)$}{\n\t\\Return \\City(x, y) \\;\n}\n\\caption{f}\n\\end{algorithm}\n\n\\end{document}"

    , "write full program"
        ~: let res = execWriter $ runReaderT (writeLatex (Program (Just (ProgramDescription "Two decimal numbers x and y." "A new struct with x and y as latitude and longiture values."))
                                                        [StructDecl "City" [Argument "lat" "double", Argument "lon" "double"]]
                                                        [Function "f" [Argument "x" "int", Argument "y" "int"] [Return (StructExpr (Struct "City" [VariableExp "x", VariableExp "y"]))]]
                                                        (Just (FunctionCall "f" [Constant (Decimal 68.04), Constant (Decimal 16.08)])))) ([], [])
           in res
        ~?= "\\documentclass{standalone}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath,commath} \n\\usepackage[linesnumbered, ruled]{algorithm2e}\n\\SetKwProg{proc}{Procedure}{}{}\n\\DontPrintSemicolon\n\\renewcommand{\\thealgocf}{}\n\\begin{document}\n\n\\begin{algorithm}[H]\n\\KwIn{Two decimal numbers x and y.}\n\\KwOut{A new struct with x and y as latitude and longiture values.}\n\\proc{$\\f(x, y)$}{\n\t\\Return \\City(x, y) \\;\n}\n\\caption{f}\n\\end{algorithm}\n\n\\end{document}"
    ]

testStatements :: Test
testStatements = TestList
    [ "write assignment with variable target"
        ~: let res = execWriter $ runReaderT (writeStmt (Assignment (VariableTarget "var'") (ExpressionValue (Constant (Decimal 20.24)))) 0) ([], [])
           in res
        ~?= "\\texttt{var'} $\\gets$ 20.24 \\;"

    , "write assignment with list index target"
        ~: let res = execWriter $ runReaderT (writeStmt (Assignment (ListIndexTarget "people" [Constant (Number 20)]) (StructValue (Struct "Person" [Constant (Text "Giuliano"), Constant (Number 70)]))) 0) ([], [])
           in res
        ~?= "people[20] $\\gets$ \\Person(\"Giuliano\", 70) \\;"

    , "write assignment with struct field target"
        ~: let res = execWriter $ runReaderT (writeStmt (Assignment (StructFieldTarget (StructField (VariableExp "sicilia") (VariableExp "breakfast"))) (ExpressionValue (Constant (Text "cornetto")))) 0) ([], [])
           in res
        ~?= "$sicilia_{breakfast}$ $\\gets$ \"cornetto\" \\;"

    , "write while loop"
        ~: let res = execWriter $ runReaderT (writeStmt (Loop (BinaryExp LessThan (VariableExp "x") (Constant (Number 10)))
                                            [Assignment (VariableTarget "x") (ExpressionValue (BinaryExp Plus (VariableExp "x") (Constant (Number 1))))]) 0) ([], [])
           in res
        ~?= "\\While{$x < 10$}{\n\t\\texttt{x} $\\gets$ x + 1 \\;\n}"

    , "write if without else"
        ~: let res = execWriter $ runReaderT (writeStmt (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                                            [Return (Constant (Boolean True))]
                                            Nothing) 0) ([], [])
           in res
        ~?= "\\uIf{$x = 10$}{\n\t\\Return \\KwTrue \\;\n}"

    , "write if with else if"
        ~: let res = execWriter $ runReaderT (writeStmt (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                                            [Return (Constant (Boolean True))]
                                            (Just (ElseIf (Constant (Boolean True)) [Return (Constant (Boolean False))] Nothing))) 0) ([], [])
           in res
        ~?= "\\uIf{$x = 10$}{\n\t\\Return \\KwTrue \\;\n}\n\\uElseIf{$\\KwTrue$}{\n\t\\Return \\KwFalse \\;\n}"

    , "write if with else"
        ~: let res = execWriter $ runReaderT (writeStmt (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                                            [Return (Constant (Boolean True))]
                                            (Just (Else [Return (Constant (Boolean False))]))) 0) ([], [])
           in res
        ~?= "\\uIf{$x = 10$}{\n\t\\Return \\KwTrue \\;\n}\n\\uElse{\n\t\\Return \\KwFalse \\;\n}"

    , "write foreach loop"
        ~: let res = execWriter $ runReaderT (writeStmt (ForEach "team" (VariableExp "teams")
                                            [CallStmt (FunctionCall "print" [VariableExp "team"])]) 0) ([], [])
           in res
        ~?= "\\For{$team \\in teams$}{\n\t\\print(team) \\;\n}"

    , "write for loop"
        ~: let res = execWriter $ runReaderT (writeStmt (For "index" (Constant (Number 0)) (Constant (Number 10))
                                            [CallStmt (FunctionCall "print" [VariableExp "index"])]) 0) ([], [])
           in res
        ~?= "\\For{$index \\gets 0$ \\KwTo $10$}{\n\t\\print(index) \\;\n}"

    , "write function call without arguments"
        ~: let res = execWriter $ runReaderT (writeStmt (CallStmt (FunctionCall "f" [])) 0) ([], [])
           in res
        ~?= "\\f() \\;"

    , "write function call with one argument"
        ~: let res = execWriter $ runReaderT (writeStmt (CallStmt (FunctionCall "f" [Constant (Number 1)])) 0) ([], [])
           in res
        ~?= "\\f(1) \\;"

    , "write function call with multiple arguments"
        ~: let res = execWriter $ runReaderT (writeStmt (CallStmt (FunctionCall "f" [Constant (Number 1), Constant (Text "Palermo"), Constant (Decimal 45.72), Constant (List [])])) 0) ([], [])
           in res
        ~?= "\\f(1, \"Palermo\", 45.72, []) \\;"

    , "write returning variable"
        ~: let res = execWriter $ runReaderT (writeStmt (Return (VariableExp "result")) 0) ([], [])
           in res
        ~?= "\\Return result \\;"

    , "write returning list"
        ~: let res = execWriter $ runReaderT (writeStmt (Return (Constant (List [Constant (Number 1), Constant (Number 2)]))) 0) ([], [])
           in res
        ~?= "\\Return [1, 2] \\;"

    , "write returning struct"
        ~: let res = execWriter $ runReaderT (writeStmt (Return (StructExpr (Struct "Person" [VariableExp "x", VariableExp "y"]))) 0) ([], [])
           in res
        ~?= "\\Return \\Person(x, y) \\;"

    , "write hash stmt"
        ~: let res = execWriter $ runReaderT (writeStmt (HashStmt (Return (VariableExp "x"))) 0) ([], [])
           in res
        ~?= ""

    , "write annotation stmt"
        ~: let res = execWriter $ runReaderT (writeStmt (AnnotationStmt "p <- choosePivot(liste)" [Assignment (VariableTarget "p") (ExpressionValue (ListIndex "liste" [Constant (Number 0)]))]) 0) ([], [])
           in res
        ~?= "\\text{p <- choosePivot(liste)} \\;"

    , "write stmt"
        ~: let res = execWriter $ runReaderT (writeStmt Break 0) ([], [])
           in res
        ~?= "\\KwBreak \\;"

    , "write stmt"
        ~: let res = execWriter $ runReaderT (writeStmt Continue 0) ([], [])
           in res
        ~?= "\\KwContinue \\;"
    ]


testExpressions :: Test
testExpressions = TestList
    [ "write function call without arguments"
        ~: let res = execWriter $ runReaderT (writeExpr (CallExp (FunctionCall "f" [])) False) ([], [])
           in res
        ~?= "\\f()"

    , "write function call with one argument"
        ~: let res = execWriter $ runReaderT (writeExpr (CallExp (FunctionCall "f" [Constant (Number 1)])) False) ([], [])
           in res
        ~?= "\\f(1)"

    , "write function call with multiple arguments"
        ~: let res = execWriter $ runReaderT (writeExpr (CallExp (FunctionCall "f" [Constant (Number 1), Constant (Text "Palermo"), Constant (Decimal 45.72), Constant (List [])])) False) ([], [])
           in res
        ~?= "\\f(1, \"Palermo\", 45.72, [])"

    , "write list index access"
        ~: let res = execWriter $ runReaderT (writeExpr (ListIndex "list" [Constant (Number 1)]) False) ([], [])
           in res
        ~?= "list[1]"

    , "write nested list index access"
        ~: let res = execWriter $ runReaderT (writeExpr (ListIndex "list" [Constant (Number 1), Constant (Number 2), Constant (Number 3)]) False) ([], [])
           in res
        ~?= "list[1][2][3]"

    , "write structfield access"
        ~: let res = execWriter $ runReaderT (writeExpr (StructFieldExp (StructField (VariableExp "struct") (VariableExp "field"))) False) ([], [])
           in res
        ~?= "$struct_{field}$"

    , "write nested structfield access"
        ~: let res = execWriter $ runReaderT (writeExpr (StructFieldExp (StructField (VariableExp "struct") (StructFieldExp (StructField (VariableExp "field") (VariableExp "nested"))))) False) ([], [])
           in res
        ~?= "$struct_{field_{nested}}$"

    , "write struct expression without arguments"
        ~: let res = execWriter $ runReaderT (writeExpr (StructExpr (Struct "Person" [])) False) ([], [])
           in res
        ~?= "\\Person()"

    , "write struct expression with one argument"
        ~: let res = execWriter $ runReaderT (writeExpr (StructExpr (Struct "Person" [Constant (Number 1)])) False) ([], [])
           in res
        ~?= "\\Person(1)"

    , "write struct expression with multiple arguments"
        ~: let res = execWriter $ runReaderT (writeExpr (StructExpr (Struct "Person" [Constant (Number 1), Constant (Text "Sicilia"), StructExpr (Struct "Country" [Constant (Text "Italia")])])) False) ([], [])
           in res
        ~?= "\\Person(1, \"Sicilia\", \\Country(\"Italia\"))"

    , "write simple variable name"
        ~: let res = execWriter $ runReaderT (writeExpr (VariableExp "variable") False) ([], [])
           in res
        ~?= "variable"

    , "write variable name with symbol"
        ~: let res = execWriter $ runReaderT (writeExpr (VariableExp "variable'") False) ([], [])
           in res
        ~?= "variable'"

    , "write negated function call"
        ~: let res = execWriter $ runReaderT (writeExpr (Not (CallExp (FunctionCall "f" [Constant (Number 1)]))) False) ([], [])
           in res
        ~?= "\\KwNot \\f(1)"

    , "write negated list index access"
        ~: let res = execWriter $ runReaderT (writeExpr (Not (ListIndex "list" [Constant (Number 1)])) False) ([], [])
           in res
        ~?= "\\KwNot list[1]"

    , "write negated nested list index access"
        ~: let res = execWriter $ runReaderT (writeExpr (Not (ListIndex "list" [Constant (Number 1), Constant (Number 2), Constant (Number 3)])) False) ([], [])
           in res
        ~?= "\\KwNot list[1][2][3]"

    , "write negated struct expression"
        ~: let res = execWriter $ runReaderT (writeExpr (Not (StructExpr (Struct "Person" [Constant (Number 1)]))) False) ([], [])
           in res
        ~?= "\\KwNot \\Person(1)"

    , "write negated variable"
        ~: let res = execWriter $ runReaderT (writeExpr (Not (VariableExp "variable")) False) ([], [])
           in res
        ~?= "\\KwNot variable"

    , "write text constant"
        ~: let res = execWriter $ runReaderT (writeExpr (Constant (Text "var")) False) ([], [])
           in res
        ~?= "\"var\""

    , "write number constant"
        ~: let res = execWriter $ runReaderT (writeExpr (Constant (Number 1905)) False) ([], [])
           in res
        ~?= "1905"

    , "write set constant"
        ~: let res = execWriter $ runReaderT (writeExpr (Constant (HashSet (Set.fromList [Constant (Number 2), Constant (Number 3), Constant (Number 2)]))) False) ([], [])
           in res
        ~?= "\\{2, 3\\}"
    ]

testProgramDescriptions :: Test
testProgramDescriptions = TestList
    [ "write empty program description"
        ~: let res = execWriter $ runReaderT (writeProgramDescription (Just (ProgramDescription "" ""))) ([], [])
           in res
        ~?= "\\KwIn{}\n\\KwOut{}\n"

    , "write random program description"
        ~: let res = execWriter $ runReaderT (writeProgramDescription (Just (ProgramDescription "description of input" "description of output"))) ([], [])
           in res
        ~?= "\\KwIn{description of input}\n\\KwOut{description of output}\n"
    ]

testValues :: Test
testValues = TestList
    [ "write small number"
        ~: let res = execWriter $ runReaderT (writeValue (Number 5)) ([], [])
           in res
        ~?= "5"

    , "write big number"
        ~: let res = execWriter $ runReaderT (writeValue (Number 42174871982315980219241)) ([], [])
           in res
        ~?= "42174871982315980219241"

    , "write nil"
        ~: let res = execWriter $ runReaderT (writeValue Nil) ([], [])
           in res
        ~?= "\\KwNil"

    , "write positive boolean"
        ~: let res = execWriter $ runReaderT (writeValue (Boolean True)) ([], [])
           in res
        ~?= "\\KwTrue"

    , "write negative boolean"
        ~: let res = execWriter $ runReaderT (writeValue (Boolean False)) ([], [])
           in res
        ~?= "\\KwFalse"

    , "write decimal"
        ~: let res = execWriter $ runReaderT (writeValue (Decimal 3.14)) ([], [])
           in res
        ~?= "3.14"

    , "write empty text"
        ~: let res = execWriter $ runReaderT (writeValue (Text "")) ([], [])
           in res
        ~?= "\"\""

    , "write regular text"
        ~: let res = execWriter $ runReaderT (writeValue (Text "informatics")) ([], [])
           in res
        ~?= "\"informatics\""

    , "write text with spaces"
        ~: let res = execWriter $ runReaderT (writeValue (Text "I love informatics")) ([], [])
           in res
        ~?= "\"I love informatics\""

    , "write empty list"
        ~: let res = execWriter $ runReaderT (writeValue (List [])) ([], [])
           in res
        ~?= "[]"

    , "write list with numbers"
        ~: let res = execWriter $ runReaderT (writeValue (List [Constant (Number 1), Constant (Number 2), Constant (Number 3)])) ([], [])
           in res
        ~?= "[1, 2, 3]"

    , "write nested list"
        ~: let res = execWriter $ runReaderT (writeValue (List [Constant (Number 1), Constant (List [Constant (List [Constant (Number 2)]), Constant (List [Constant (Number 3)])])])) ([], [])
           in res
        ~?= "[1, [[2], [3]]]"

    , "write empty hashset"
        ~: let res = execWriter $ runReaderT (writeValue (HashSet (Set.fromList []))) ([], [])
           in res
        ~?= "\\{\\}"

    , "write hashset"
        ~: let res = execWriter $ runReaderT (writeValue (HashSet (Set.fromList [Constant (Number 1), Constant (Number 2), Constant (Number 1), Constant (Number 2), Constant (Number 1), Constant (Number 2)]))) ([], [])
           in res
        ~?= "\\{1, 2\\}"

    , "write empty hashmap"
        ~: let res = execWriter $ runReaderT (writeValue (HashMap (Map.fromList []))) ([], [])
           in res
        ~?= "\\{\\}"

    , "write hashmap with string keys"
        ~: let res = execWriter $ runReaderT (writeValue (HashMap (Map.fromList [(Constant (Text "key"), Constant (Text "value")), (Constant (Text "goals"), Constant (Number 123))]))) ([], [])
           in res
        ~?= "\\{\"goals\": 123, \"key\": \"value\"\\}"

    , "write hashmap with different type keys"
        ~: let res = execWriter $ runReaderT (writeValue (HashMap (Map.fromList [(Constant (Number 1), Constant (Text "value")), (Constant (Decimal 2.5), Constant (Number 123))]))) ([], [])
           in res
        ~?= "\\{1: \"value\", 2.5: 123\\}"
    ]
