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

config :: String
config = "\\documentclass{standalone}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath,commath} \n\\usepackage[linesnumbered, ruled]{algorithm2e}\n\\SetKwProg{proc}{Procedure}{}{}\n\\DontPrintSemicolon\n\\renewcommand{\\thealgocf}{}\n\\begin{document}\n\n\\begin{algorithm}[H]\n"

testPseudocodeWriter :: Test
testPseudocodeWriter = TestList
    [ testValues
    , testProgramDescription
    , testExpressions
    , testStatements
    , testProgram
    ]

testProgram :: Test
testProgram = TestList
    [ "write empty program"
        ~: (execWriter $ runReaderT (writeLatex (Program Nothing [] [] Nothing)) ([], []))
        ~?= ""

    , "write program with just description"
        ~: (execWriter $ runReaderT (writeLatex (Program (Just (ProgramDescription "There is no program." "Thus nothing is returned."))
                                                                  []
                                                                  []
                                                                  Nothing)) ([], []))
        ~?= ""

    , "write program with just struct"
        ~: (execWriter $ runReaderT (writeLatex (Program Nothing
                                                                  [StructDecl "Person" [Argument "age" "int"]]
                                                                  []
                                                                  Nothing)) ([], []))
        ~?= ""

    , "write program with just function declaration"
        ~: (execWriter $ runReaderT (writeLatex (Program Nothing
                                                                  []
                                                                  [FunctionDecl "f" [] [Return (Constant (Number 1))]]
                                                                  Nothing)) ([], []))
        ~?= config ++ "\\proc{$\\f()$}{\n\t\\Return 1 \\;\n}\n\\caption{f}\n\\end{algorithm}\n\n\\end{document}"

    , "write simple program"
        ~: (execWriter $ runReaderT (writeLatex (Program Nothing
                                                                  []
                                                                  [FunctionDecl "f" [Argument "x" "int", Argument "y" "int"] [Return (BinaryExp Plus (VariableExp "x") (VariableExp "y"))]]
                                                                  (Just (FunctionCall "f" [Constant (Number 1), Constant (Number 2)])))) ([], []))
        ~?= config ++ "\\proc{$\\f(x, y)$}{\n\t\\Return x $+$ y \\;\n}\n\\caption{f}\n\\end{algorithm}\n\n\\end{document}"

    , "write program with struct"
        ~: (execWriter $ runReaderT (writeLatex (Program Nothing
                                                                  [StructDecl "City" [Argument "lat" "double", Argument "lon" "double"]]
                                                                  [FunctionDecl "f" [Argument "x" "int", Argument "y" "int"] [Return (StructExpr (Struct "City" [VariableExp "x", VariableExp "y"]))]]
                                                                  (Just (FunctionCall "f" [Constant (Decimal 68.04), Constant (Decimal 16.08)])))) ([], []))
        ~?= config ++ "\\proc{$\\f(x, y)$}{\n\t\\Return \\City(x, y) \\;\n}\n\\caption{f}\n\\end{algorithm}\n\n\\end{document}"

    , "write full program"
        ~: (execWriter $ runReaderT (writeLatex (Program (Just (ProgramDescription "Two decimal numbers x and y." "A new struct with x and y as latitude and longiture values."))
                                                                  [StructDecl "City" [Argument "lat" "double", Argument "lon" "double"]]
                                                                  [FunctionDecl "f" [Argument "x" "int", Argument "y" "int"] [Return (StructExpr (Struct "City" [VariableExp "x", VariableExp "y"]))]]
                                                                  (Just (FunctionCall "f" [Constant (Decimal 68.04), Constant (Decimal 16.08)])))) ([], []))
        ~?= config ++ "\\KwIn{Two decimal numbers x and y.}\n\\KwOut{A new struct with x and y as latitude and longiture values.}\n\\proc{$\\f(x, y)$}{\n\t\\Return \\City(x, y) \\;\n}\n\\caption{f}\n\\end{algorithm}\n\n\\end{document}"
    ]

testStatements :: Test
testStatements = TestList
    [ "write assignment with variable target"
        ~: (execWriter $ runReaderT (writeStmt (Assignment (VariableTarget "var'") (ExpressionValue (Constant (Decimal 20.24)))) 0) ([], []))
        ~?= "\\texttt{var'} $\\gets$ 20.24 \\;"

    , "write assignment with list index target"
        ~: (execWriter $ runReaderT (writeStmt (Assignment (ListIndexTarget "people" [Constant (Number 20)]) (StructValue (Struct "Person" [Constant (Text "Giuliano"), Constant (Number 70)]))) 0) ([], []))
        ~?= "people[20] $\\gets$ \\Person(\"Giuliano\", 70) \\;"

    , "write assignment with struct field target"
        ~: (execWriter $ runReaderT (writeStmt (Assignment (StructFieldTarget (StructField (VariableExp "sicilia") (VariableExp "breakfast"))) (ExpressionValue (Constant (Text "cornetto")))) 0) ([], []))
        ~?= "$sicilia_{breakfast}$ $\\gets$ \"cornetto\" \\;"

    , "write while loop"
        ~: (execWriter $ runReaderT (writeStmt (Loop (BinaryExp LessThan (VariableExp "x") (Constant (Number 10)))
                                            [Assignment (VariableTarget "x") (ExpressionValue (BinaryExp Plus (VariableExp "x") (Constant (Number 1))))]) 0) ([], []))
        ~?= "\\While{$x < 10$}{\n\t\\texttt{x} $\\gets$ x $+$ 1 \\;\n}"

    , "write if without else"
        ~: (execWriter $ runReaderT (writeStmt (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                                            [Return (Constant (Boolean True))]
                                            Nothing) 0) ([], []))
        ~?= "\\uIf{$x = 10$}{\n\t\\Return \\KwTrue \\;\n}"

    , "write if with else if"
        ~: (execWriter $ runReaderT (writeStmt (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                                            [Return (Constant (Boolean True))]
                                            (Just (ElseIf (Constant (Boolean True)) [Return (Constant (Boolean False))] Nothing))) 0) ([], []))
        ~?= "\\uIf{$x = 10$}{\n\t\\Return \\KwTrue \\;\n}\n\\uElseIf{$\\KwTrue$}{\n\t\\Return \\KwFalse \\;\n}"

    , "write if with else"
        ~: (execWriter $ runReaderT (writeStmt (If (BinaryExp Equal (VariableExp "x") (Constant (Number 10)))
                                            [Return (Constant (Boolean True))]
                                            (Just (Else [Return (Constant (Boolean False))]))) 0) ([], []))
        ~?= "\\uIf{$x = 10$}{\n\t\\Return \\KwTrue \\;\n}\n\\uElse{\n\t\\Return \\KwFalse \\;\n}"

    , "write foreach loop"
        ~: (execWriter $ runReaderT (writeStmt (ForEach "team" (VariableExp "teams")
                                            [CallStmt (FunctionCall "print" [VariableExp "team"])]) 0) ([], []))
        ~?= "\\For{$team \\in teams$}{\n\t\\print(team) \\;\n}"

    , "write for loop"
        ~: (execWriter $ runReaderT (writeStmt (For "index" (Constant (Number 0)) (Constant (Number 10))
                                            [CallStmt (FunctionCall "print" [VariableExp "index"])]) 0) ([], []))
        ~?= "\\For{$index \\gets 0$ \\KwTo $10$}{\n\t\\print(index) \\;\n}"

    , "write function call without arguments"
        ~: (execWriter $ runReaderT (writeStmt (CallStmt (FunctionCall "f" [])) 0) ([], []))
        ~?= "\\f() \\;"

    , "write function call with one argument"
        ~: (execWriter $ runReaderT (writeStmt (CallStmt (FunctionCall "f" [Constant (Number 1)])) 0) ([], []))
        ~?= "\\f(1) \\;"

    , "write function call with multiple arguments"
        ~: (execWriter $ runReaderT (writeStmt (CallStmt (FunctionCall "f" [Constant (Number 1), Constant (Text "Palermo"), Constant (Decimal 45.72), Constant (List [])])) 0) ([], []))
        ~?= "\\f(1, \"Palermo\", 45.72, []) \\;"

    , "write returning variable"
        ~: (execWriter $ runReaderT (writeStmt (Return (VariableExp "result")) 0) ([], []))
        ~?= "\\Return result \\;"

    , "write returning list"
        ~: (execWriter $ runReaderT (writeStmt (Return (Constant (List [Constant (Number 1), Constant (Number 2)]))) 0) ([], []))
        ~?= "\\Return [1, 2] \\;"

    , "write returning struct"
        ~: (execWriter $ runReaderT (writeStmt (Return (StructExpr (Struct "Person" [VariableExp "x", VariableExp "y"]))) 0) ([], []))
        ~?= "\\Return \\Person(x, y) \\;"

    , "write hash stmt"
        ~: (execWriter $ runReaderT (writeStmt (HashStmt (Return (VariableExp "x"))) 0) ([], []))
        ~?= ""

    , "write annotation stmt"
        ~: (execWriter $ runReaderT (writeStmt (AnnotationStmt "p <- choosePivot(liste)" [Assignment (VariableTarget "p") (ExpressionValue (ListIndex "liste" [Constant (Number 0)]))]) 0) ([], []))
        ~?= "\\text{p <- choosePivot(liste)} \\;"

    , "write break stmt"
        ~: (execWriter $ runReaderT (writeStmt Break 0) ([], []))
        ~?= "\\KwBreak \\;"

    , "write continue stmt"
        ~: (execWriter $ runReaderT (writeStmt Continue 0) ([], []))
        ~?= "\\KwContinue \\;"
    ]


testExpressions :: Test
testExpressions = TestList
    [ "write function call without arguments"
        ~: (execWriter $ runReaderT (writeExpr (CallExp (FunctionCall "f" [])) False) ([], []))
        ~?= "\\f()"

    , "write function call with one argument"
        ~: (execWriter $ runReaderT (writeExpr (CallExp (FunctionCall "f" [Constant (Number 1)])) False) ([], []))
        ~?= "\\f(1)"

    , "write function call with multiple arguments"
        ~: (execWriter $ runReaderT (writeExpr (CallExp (FunctionCall "f" [Constant (Number 1), Constant (Text "Palermo"), Constant (Decimal 45.72), Constant (List [])])) False) ([], []))
        ~?= "\\f(1, \"Palermo\", 45.72, [])"

    , "write list index access"
        ~: (execWriter $ runReaderT (writeExpr (ListIndex "list" [Constant (Number 1)]) False) ([], []))
        ~?= "list[1]"

    , "write nested list index access"
        ~: (execWriter $ runReaderT (writeExpr (ListIndex "list" [Constant (Number 1), Constant (Number 2), Constant (Number 3)]) False) ([], []))
        ~?= "list[1][2][3]"

    , "write structfield access"
        ~: (execWriter $ runReaderT (writeExpr (StructFieldExp (StructField (VariableExp "struct") (VariableExp "field"))) False) ([], []))
        ~?= "$struct_{field}$"

    , "write nested structfield access"
        ~: (execWriter $ runReaderT (writeExpr (StructFieldExp (StructField (VariableExp "struct") (StructFieldExp (StructField (VariableExp "field") (VariableExp "nested"))))) False) ([], []))
        ~?= "$struct_{field_{nested}}$"

    , "write struct expression without arguments"
        ~: (execWriter $ runReaderT (writeExpr (StructExpr (Struct "Person" [])) False) ([], []))
        ~?= "\\Person()"

    , "write struct expression with one argument"
        ~: (execWriter $ runReaderT (writeExpr (StructExpr (Struct "Person" [Constant (Number 1)])) False) ([], []))
        ~?= "\\Person(1)"

    , "write struct expression with multiple arguments"
        ~: (execWriter $ runReaderT (writeExpr (StructExpr (Struct "Person" [Constant (Number 1), Constant (Text "Sicilia"), StructExpr (Struct "Country" [Constant (Text "Italia")])])) False) ([], []))
        ~?= "\\Person(1, \"Sicilia\", \\Country(\"Italia\"))"

    , "write simple variable name"
        ~: (execWriter $ runReaderT (writeExpr (VariableExp "variable") False) ([], []))
        ~?= "variable"

    , "write variable name with symbol"
        ~: (execWriter $ runReaderT (writeExpr (VariableExp "variable'") False) ([], []))
        ~?= "variable'"

    , "write negated function call"
        ~: (execWriter $ runReaderT (writeExpr (Not (CallExp (FunctionCall "f" [Constant (Number 1)]))) False) ([], []))
        ~?= "\\KwNot \\f(1)"

    , "write negated list index access"
        ~: (execWriter $ runReaderT (writeExpr (Not (ListIndex "list" [Constant (Number 1)])) False) ([], []))
        ~?= "\\KwNot list[1]"

    , "write negated nested list index access"
        ~: (execWriter $ runReaderT (writeExpr (Not (ListIndex "list" [Constant (Number 1), Constant (Number 2), Constant (Number 3)])) False) ([], []))
        ~?= "\\KwNot list[1][2][3]"

    , "write negated struct expression"
        ~: (execWriter $ runReaderT (writeExpr (Not (StructExpr (Struct "Person" [Constant (Number 1)]))) False) ([], []))
        ~?= "\\KwNot \\Person(1)"

    , "write negated variable"
        ~: (execWriter $ runReaderT (writeExpr (Not (VariableExp "variable")) False) ([], []))
        ~?= "\\KwNot variable"

    , "write text constant"
        ~: (execWriter $ runReaderT (writeExpr (Constant (Text "var")) False) ([], []))
        ~?= "\"var\""

    , "write number constant"
        ~: (execWriter $ runReaderT (writeExpr (Constant (Number 1905)) False) ([], []))
        ~?= "1905"

    , "write set constant"
        ~: (execWriter $ runReaderT (writeExpr (Constant (HashSet (Set.fromList [Constant (Number 2), Constant (Number 3), Constant (Number 2)]))) False) ([], []))
        ~?= "\\{2, 3\\}"
    ]

testProgramDescription :: Test
testProgramDescription = TestList
    [ "write empty program description"
        ~: (execWriter $ runReaderT (writeProgramDescription (Just (ProgramDescription "" ""))) ([], []))
        ~?= "\\KwIn{}\n\\KwOut{}\n"

    , "write random program description"
        ~: (execWriter $ runReaderT (writeProgramDescription (Just (ProgramDescription "description of input" "description of output"))) ([], []))
        ~?= "\\KwIn{description of input}\n\\KwOut{description of output}\n"
    ]

testValues :: Test
testValues = TestList
    [ "write small number"
        ~: (execWriter $ runReaderT (writeValue (Number 5)) ([], []))
        ~?= "5"

    , "write big number"
        ~: (execWriter $ runReaderT (writeValue (Number 42174871982315980219241)) ([], []))
        ~?= "42174871982315980219241"

    , "write nil"
        ~: (execWriter $ runReaderT (writeValue Nil) ([], []))
        ~?= "\\KwNil"

    , "write positive boolean"
        ~: (execWriter $ runReaderT (writeValue (Boolean True)) ([], []))
        ~?= "\\KwTrue"

    , "write negative boolean"
        ~: (execWriter $ runReaderT (writeValue (Boolean False)) ([], []))
        ~?= "\\KwFalse"

    , "write decimal"
        ~: (execWriter $ runReaderT (writeValue (Decimal 3.14)) ([], []))
        ~?= "3.14"

    , "write empty text"
        ~: (execWriter $ runReaderT (writeValue (Text "")) ([], []))
        ~?= "\"\""

    , "write regular text"
        ~: (execWriter $ runReaderT (writeValue (Text "informatics")) ([], []))
        ~?= "\"informatics\""

    , "write text with spaces"
        ~: (execWriter $ runReaderT (writeValue (Text "I love informatics")) ([], []))
        ~?= "\"I love informatics\""

    , "write empty list"
        ~: (execWriter $ runReaderT (writeValue (List [])) ([], []))
        ~?= "[]"

    , "write list with numbers"
        ~: (execWriter $ runReaderT (writeValue (List [Constant (Number 1), Constant (Number 2), Constant (Number 3)])) ([], []))
        ~?= "[1, 2, 3]"

    , "write nested list"
        ~: (execWriter $ runReaderT (writeValue (List [Constant (Number 1), Constant (List [Constant (List [Constant (Number 2)]), Constant (List [Constant (Number 3)])])])) ([], []))
        ~?= "[1, [[2], [3]]]"

    , "write empty hashset"
        ~: (execWriter $ runReaderT (writeValue (HashSet (Set.fromList []))) ([], []))
        ~?= "\\{\\}"

    , "write hashset"
        ~: (execWriter $ runReaderT (writeValue (HashSet (Set.fromList [Constant (Number 1), Constant (Number 2), Constant (Number 1), Constant (Number 2), Constant (Number 1), Constant (Number 2)]))) ([], []))
        ~?= "\\{1, 2\\}"

    , "write empty hashmap"
        ~: (execWriter $ runReaderT (writeValue (HashMap (Map.fromList []))) ([], []))
        ~?= "\\{\\}"

    , "write hashmap with string keys"
        ~: (execWriter $ runReaderT (writeValue (HashMap (Map.fromList [(Constant (Text "key"), Constant (Text "value")), (Constant (Text "goals"), Constant (Number 123))]))) ([], []))
        ~?= "\\{\"goals\": 123, \"key\": \"value\"\\}"

    , "write hashmap with different type keys"
        ~: (execWriter $ runReaderT (writeValue (HashMap (Map.fromList [(Constant (Number 1), Constant (Text "value")), (Constant (Decimal 2.5), Constant (Number 123))]))) ([], []))
        ~?= "\\{1: \"value\", 2.5: 123\\}"
    ]
