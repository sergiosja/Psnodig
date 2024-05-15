module Writers.Flowchart (testFlowchartWriter) where

import Syntax
import LaTeX.Flowcharts
    ( Environment(..)
    , drawFlowchart
    , drawFunctionDecl
    , drawStmts
    , drawStartstopNode
    , drawStatementNode
    , drawExpr
    , drawDecisionNode
    , lastElseIsReturn
    , stmtCount
    , drawOpCrementHelper
    )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Writer
import Control.Monad.State
import Test.HUnit

config :: String
config =
    "\\documentclass[margin=3mm]{standalone}\n\\usepackage{tikz}\n\\usetikzlibrary{shapes, arrows}\n\n" ++
    "\\tikzstyle{startstop} = [rectangle, rounded corners, minimum width=1cm, minimum height=1cm, text centered, draw=black, text=white, fill=black!80]\n" ++
    "\\tikzstyle{statement} = [rectangle, minimum width=1cm, minimum height=1cm, text centered, draw=black, fill=blue!20]\n" ++
    "\\tikzstyle{decision} = [rectangle, minimum height=1cm, text centered, draw=black, fill=yellow!30]\n" ++
    "\\tikzstyle{edge} = [thick, ->, >=stealth]\n\n\\begin{document}\n\\begin{tikzpicture}[node distance=2cm]\n\n"

env :: Environment
env = (Environment [] 0 [0] [])


testFlowchartWriter :: Test
testFlowchartWriter = TestList
    [ testFunctionDecl
    , testStmts
    , testNodes
    , testLastElseIsReturn
    , testStmtCount
    , testOpCrementHelper
    , testExpressions
    , testProgram
    ]


testProgram :: Test
testProgram = TestList
    [ "draw empty program"
        ~: (execWriter $ runStateT (drawFlowchart (Program Nothing [] [] Nothing)) env)
        ~?= ""

    , "draw program with while loop"
        ~: (execWriter $ runStateT (drawFlowchart (Program Nothing
                []
                [FunctionDecl "f" [] [Assignment (VariableTarget "x") (ExpressionValue (Constant (Number 0))), Loop (BinaryExp GreaterThan (VariableExp "x") (Constant (Number 5))) [CallStmt (FunctionCall "print" [VariableExp "x"]), Assignment (VariableTarget "x") (ExpressionValue (BinaryExp Plus (VariableExp "x") (Constant (Number 1))))]]]
                Nothing)) env)
        ~?= config ++ "\\node (0) [startstop] {f()};\n\\node (1) [statement, below of=0] {x $\\gets$ 0};\n\\node (2) [decision, below of=1] {x $>$ 5 ?};\n\\node (3) [statement, yshift=-1cm, xshift=-4cm, below left of=2] {print(x)};\n\\node (4) [statement, below of=3] {x $\\gets$ x + 1};\n\n\\draw [edge] (0) -- (1);\n\\draw [edge] (1) -- (2);\n\\draw [edge] (2) -- node[anchor=east, yshift=0.1cm]{true} (3);\n\\draw [edge] (3) -- (4);\n\\draw [edge] (4) -| (2);\n\n\\end{tikzpicture}\n\\end{document}"

    , "draw program with for loop"
        ~: (execWriter $ runStateT (drawFlowchart (Program Nothing
                []
                [FunctionDecl "f" [Argument "pointless" "list"] [For "i" (Constant (Number 0)) (Constant (Number 100)) [If (CallExp (FunctionCall "in" [VariableExp "i", VariableExp "pointless"])) [CallStmt (FunctionCall "print" [Constant (Text "Fail: ")])] (Just (Else [CallStmt (FunctionCall "print" [Constant (Text "Success: ")])])), CallStmt (FunctionCall "print" [VariableExp "i"]), CallStmt (FunctionCall "append" [VariableExp "i", VariableExp "pointless"])], Return (VariableExp "pointless")]]
                Nothing)) env)
        ~?= config ++ "\\node (0) [startstop] {f(pointless)};\n\\node (1) [statement, below of=0] {i $\\gets$ 0};\n\\node (2) [decision, below of=1] {i $<$ 100 ?};\n\\node (3) [decision, yshift=-0.5cm, xshift=1.5cm, below right of=2] {in(i, pointless) ?};\n\\node (4) [statement, yshift=-0.5cm, xshift=-1.5cm, below left of=3] {print(\"Fail: \")};\n\\node (5) [statement, yshift=-0.5cm, xshift=1.5cm, below right of=3] {print(\"Success: \")};\n\\node (6) [statement, below of=5] {print(i)};\n\\node (7) [statement, below of=6] {append(i, pointless)};\n\\node (8) [statement, xshift=3cm, below right of=7] {i $\\gets$ i + 1};\n\\node (9) [startstop, yshift=-0.5cm, xshift=-1.5cm, below left of=2] {pointless};\n\n\\draw [edge] (0) -- (1);\n\\draw [edge] (1) -- (2);\n\\draw [edge] (2) -- node[anchor=east, yshift=0.1cm]{false} (9);\n\\draw [edge] (2) -- node[anchor=west, yshift=0.1cm]{true} (3);\n\\draw [edge] (3) -- node[anchor=west, yshift=0.1cm]{false} (5);\n\\draw [edge] (3) -- node[anchor=east, yshift=0.1cm]{true} (4);\n\\draw [edge] (4) -- (6);\n\\draw [edge] (5) -- (6);\n\\draw [edge] (6) -- (7);\n\\draw [edge] (7) |- (8);\n\\draw [edge] (8) |- (2);\n\n\\end{tikzpicture}\n\\end{document}"

    , "draw program with foreach loop"
        ~: (execWriter $ runStateT (drawFlowchart (Program Nothing
                []
                [FunctionDecl "f" [] [Assignment (VariableTarget "x") (ExpressionValue (Constant (List [Constant (Number 1), Constant (Number 2), Constant (Number 3), Constant (Number 4)]))), Assignment (VariableTarget "s") (ExpressionValue (Constant (Number 0))), ForEach "i" (VariableExp "x") [Assignment (VariableTarget "s") (ExpressionValue (BinaryExp Plus (VariableExp "s") (VariableExp "i"))), CallStmt (FunctionCall "print" [VariableExp "s"])], Return (VariableExp "s")]]
                Nothing)) env)
        ~?= config ++ "\\node (0) [startstop] {f()};\n\\node (1) [statement, below of=0] {x $\\gets$ [1, 2, 3, 4]};\n\\node (2) [statement, below of=1] {s $\\gets$ 0};\n\\node (3) [decision, below of=2] {Iterated x ?};\n\\node (4) [statement, yshift=-0.5cm, xshift=1.5cm, below right of=3] {i $\\gets$ next element in x};\n\\node (5) [statement, below of=4] {s $\\gets$ s + i};\n\\node (6) [statement, below of=5] {print(s)};\n\\node (7) [startstop, yshift=-0.5cm, xshift=-1.5cm, below left of=3] {s};\n\n\\draw [edge] (0) -- (1);\n\\draw [edge] (1) -- (2);\n\\draw [edge] (2) -- (3);\n\\draw [edge] (3) -- node[anchor=east, yshift=0.1cm]{true} (7);\n\\draw [edge] (3) -- node[anchor=west, yshift=0.1cm]{false} (4);\n\\draw [edge] (4) -- (5);\n\\draw [edge] (5) -- (6);\n\\draw [edge] (6) -| (3);\n\n\\end{tikzpicture}\n\\end{document}"

    , "draw program with if stmts"
        ~: (execWriter $ runStateT (drawFlowchart (Program Nothing
                []
                [FunctionDecl "g" [Argument "x" "int",Argument "y" "int", Argument "z" "int"] [If (BinaryExp GreaterThan (VariableExp "x") (VariableExp "y")) [Return (VariableExp "x")] (Just (ElseIf (BinaryExp GreaterThan (VariableExp "y") (VariableExp "z")) [Return (VariableExp "y")] (Just (ElseIf (BinaryExp Equal (VariableExp "x") (VariableExp "z")) [Return (VariableExp "False")] Nothing)))), Return (VariableExp "z")]]
                Nothing)) env)
        ~?= config ++ "\\node (0) [startstop] {g(x, y, z)};\n\\node (1) [decision, below of=0] {x $>$ y ?};\n\\node (2) [startstop, yshift=-0.5cm, xshift=-1.5cm, below left of=1] {x};\n\\node (3) [decision, yshift=-0.5cm, xshift=1.5cm, below right of=1] {y $>$ z ?};\n\\node (4) [startstop, yshift=-0.5cm, xshift=-1.5cm, below left of=3] {y};\n\\node (5) [decision, yshift=-0.5cm, xshift=1.5cm, below right of=3] {x $=$ z ?};\n\\node (6) [startstop, yshift=-0.5cm, xshift=-1.5cm, below left of=5] {False};\n\\node (7) [startstop, yshift=-0.5cm, xshift=1.5cm, below right of=5] {z};\n\n\\draw [edge] (0) -- (1);\n\\draw [edge] (1) -- node[anchor=west, yshift=0.1cm]{false} (3);\n\\draw [edge] (1) -- node[anchor=east, yshift=0.1cm]{true} (2);\n\\draw [edge] (3) -- node[anchor=west, yshift=0.1cm]{false} (5);\n\\draw [edge] (3) -- node[anchor=east, yshift=0.1cm]{true} (4);\n\\draw [edge] (5) -- node[anchor=west, yshift=0.1cm]{false} (7);\n\\draw [edge] (5) -- node[anchor=east, yshift=0.1cm]{true} (6);\n\n\\end{tikzpicture}\n\\end{document}"
    ]

testLastElseIsReturn :: Test
testLastElseIsReturn = TestList
    [ "test last else without else"
        ~: lastElseIsReturn (Just (ElseIf (VariableExp "x") [CallStmt (FunctionCall "print" [VariableExp "x"])] (Just (ElseIf (VariableExp "x") [Return (VariableExp "x")] Nothing))))
        ~?= False

    , "test last else with return"
        ~: lastElseIsReturn (Just (ElseIf (VariableExp "x") [CallStmt (FunctionCall "print" [VariableExp "x"])] (Just (ElseIf (VariableExp "x") [Return (VariableExp "x")] (Just (Else [Return (VariableExp "x")]))))))
        ~?= True

    , "test last else without return"
        ~: lastElseIsReturn (Just (ElseIf (VariableExp "x") [CallStmt (FunctionCall "print" [VariableExp "x"])] (Just (ElseIf (VariableExp "x") [CallStmt (FunctionCall "print" [VariableExp "x"])] (Just (Else [CallStmt (FunctionCall "print" [VariableExp "x"])]))))))
        ~?= False
    ]

testStmtCount :: Test
testStmtCount = TestList
    [ "test empty stmt list"
        ~: stmtCount []
        ~?= 0

    , "test a few stmts"
        ~: stmtCount [Assignment (VariableTarget "x") (ExpressionValue (Constant (Number 5))), Assignment (StructFieldTarget (StructField (VariableExp "p") (VariableExp "age"))) (ExpressionValue (StructFieldExp (StructField (VariableExp "p") (BinaryExp Plus (VariableExp "age") (Constant (Number 1)))))), CallStmt (FunctionCall "print" [VariableExp "p"]), Return (VariableExp "x")]
        ~?= 4

    , "test many and nested stmts"
        ~: stmtCount [Assignment (VariableTarget "x") (ExpressionValue (Constant (Number 5))),Loop (BinaryExp GreaterThan (VariableExp "x") (Constant (Number 5))) [Assignment (StructFieldTarget (StructField (VariableExp "p") (VariableExp "age"))) (ExpressionValue (StructFieldExp (StructField (VariableExp "p") (BinaryExp Plus (VariableExp "age") (Constant (Number 1)))))),CallStmt (FunctionCall "print" [VariableExp "p"]),ForEach "b" (VariableExp "a") [Assignment (StructFieldTarget (StructField (VariableExp "p") (VariableExp "age"))) (ExpressionValue (StructFieldExp (StructField (VariableExp "p") (BinaryExp Plus (VariableExp "age") (Constant (Number 1)))))),CallStmt (FunctionCall "print" [VariableExp "p"])],If (VariableExp "x") [Return (VariableExp "y")] (Just (Else [Break]))],Loop (BinaryExp GreaterThan (VariableExp "x") (Constant (Number 5))) [Assignment (StructFieldTarget (StructField (VariableExp "p") (VariableExp "age"))) (ExpressionValue (StructFieldExp (StructField (VariableExp "p") (BinaryExp Plus (VariableExp "age") (Constant (Number 1)))))),CallStmt (FunctionCall "print" [VariableExp "p"]),ForEach "b" (VariableExp "a") [Assignment (StructFieldTarget (StructField (VariableExp "p") (VariableExp "age"))) (ExpressionValue (StructFieldExp (StructField (VariableExp "p") (BinaryExp Plus (VariableExp "age") (Constant (Number 1)))))),CallStmt (FunctionCall "print" [VariableExp "p"])],If (VariableExp "x") [Return (VariableExp "y")] (Just (Else [Loop (BinaryExp GreaterThan (VariableExp "x") (Constant (Number 5))) [Assignment (StructFieldTarget (StructField (VariableExp "p") (VariableExp "age"))) (ExpressionValue (StructFieldExp (StructField (VariableExp "p") (BinaryExp Plus (VariableExp "age") (Constant (Number 1)))))),CallStmt (FunctionCall "print" [VariableExp "p"]),ForEach "b" (VariableExp "a") [Assignment (StructFieldTarget (StructField (VariableExp "p") (VariableExp "age"))) (ExpressionValue (StructFieldExp (StructField (VariableExp "p") (BinaryExp Plus (VariableExp "age") (Constant (Number 1)))))),CallStmt (FunctionCall "print" [VariableExp "p"])],If (VariableExp "x") [Return (VariableExp "y")] (Just (Else [Break]))]]))]]
        ~?= 30
    ]

testExpressions :: Test
testExpressions = TestList
    [ "draw function call without arguments"
        ~: drawExpr (CallExp (FunctionCall "f" []))
        ~?= "f()"

    , "draw function call with one argument"
        ~: drawExpr (CallExp (FunctionCall "f" [Constant (Number 1)]))
        ~?= "f(1)"

    , "draw function call with multiple arguments"
        ~: drawExpr (CallExp (FunctionCall "f" [Constant (Number 1), Constant (Text "Palermo"), Constant (Decimal 45.72), Constant (List [])]))
        ~?= "f(1, \"Palermo\", 45.72, [])"

    , "draw list index access"
        ~: drawExpr (ListIndex "list" [Constant (Number 1)])
        ~?= "list[1]"

    , "draw nested list index access"
        ~: drawExpr (ListIndex "list" [Constant (Number 1), Constant (Number 2), Constant (Number 3)])
        ~?= "list[1][2][3]"

    , "draw structfield access"
        ~: drawExpr (StructFieldExp (StructField (VariableExp "struct") (VariableExp "field")))
        ~?= "struct.field"

    , "draw nested structfield access"
        ~: drawExpr (StructFieldExp (StructField (VariableExp "struct") (StructFieldExp (StructField (VariableExp "field") (VariableExp "nested")))))
        ~?= "struct.field.nested"

    , "draw struct expression without arguments"
        ~: drawExpr (StructExpr (Struct "Person" []))
        ~?= "Person()"

    , "draw struct expression with one argument"
        ~: drawExpr (StructExpr (Struct "Person" [Constant (Number 1)]))
        ~?= "Person(1)"

    , "draw struct expression with multiple arguments"
        ~: drawExpr (StructExpr (Struct "Person" [Constant (Number 1), Constant (Text "Sicilia"), StructExpr (Struct "Country" [Constant (Text "Italia")])]))
        ~?= "Person(1, \"Sicilia\", Country(\"Italia\"))"

    , "draw simple variable name"
        ~: drawExpr (VariableExp "variable")
        ~?= "variable"

    , "draw variable name with symbol"
        ~: drawExpr (VariableExp "variable'")
        ~?= "variable'"

    , "draw negated function call"
        ~: drawExpr (Not (CallExp (FunctionCall "f" [Constant (Number 1)])))
        ~?= "not f(1)"

    , "draw negated list index access"
        ~: drawExpr (Not (ListIndex "list" [Constant (Number 1)]))
        ~?= "not list[1]"

    , "draw negated nested list index access"
        ~: drawExpr (Not (ListIndex "list" [Constant (Number 1), Constant (Number 2), Constant (Number 3)]))
        ~?= "not list[1][2][3]"

    , "draw negated struct expression"
        ~: drawExpr (Not (StructExpr (Struct "Person" [Constant (Number 1)])))
        ~?= "not Person(1)"

    , "draw negated variable"
        ~: drawExpr (Not (VariableExp "variable"))
        ~?= "not variable"

    , "draw text constant"
        ~: drawExpr (Constant (Text "var"))
        ~?= "\"var\""

    , "draw number constant"
        ~: drawExpr (Constant (Number 1905))
        ~?= "1905"

    , "draw hashset constant"
        ~: drawExpr (Constant (HashSet (Set.fromList [Constant (Number 2), Constant (Number 3), Constant (Number 2)])))
        ~?= "(2, 3)"

    , "draw hashmap constant"
        ~: drawExpr (Constant (HashMap (Map.fromList [(Constant (Number 1), Constant (Text "value")), (Constant (Decimal 2.5), Constant (Number 123))])))
        ~?= "{1: \"value\", 2.5: 123}"
    ]

testOpCrementHelper :: Test
testOpCrementHelper = TestList
    [ "test positive op crement"
        ~: drawOpCrementHelper (Constant (Number 0)) (Constant (Number 10)) "x"
        ~?= (" $<$ ","x $\\gets$ x + 1")

    , "test positive op crement"
        ~: drawOpCrementHelper (Constant (Number 10)) (Constant (Number 0)) "x"
        ~?= (" $>$ ","x $\\gets$ x - 1")

    , "test default op crement"
        ~: drawOpCrementHelper (VariableExp "fra") (VariableExp "til") "x"
        ~?= (" $<$ ","x $\\gets$ x + 1")
    ]

testNodes :: Test
testNodes = TestList
    [ "draw startstop node"
        ~: (execWriter $ runStateT (drawStartstopNode "1" "below of=0" "startstop") env)
        ~?= "\\node (1) [startstop, below of=0] {startstop};\n"

    , "draw statement node"
        ~: (execWriter $ runStateT (drawStatementNode "1" "below of=0" "statement") env)
        ~?= "\\node (1) [statement, below of=0] {statement};\n"

    , "draw decision node"
        ~: (execWriter $ runStateT (drawDecisionNode "1" "below of=0" "decision") env)
        ~?= "\\node (1) [decision, below of=0] {decision ?};\n"
    ]


testStmts :: Test
testStmts = TestList
    [ "draw assignment stmt"
        ~: (execWriter $ runStateT (drawStmts [Assignment (VariableTarget "x") (ExpressionValue (Constant (Number 5)))] False) env)
        ~?= "\\node (1) [statement, below of=0] {x $\\gets$ 5};\n"

    , "draw loop stmt"
        ~: (execWriter $ runStateT (drawStmts [Loop (VariableExp "True") [CallStmt (FunctionCall "print" [Constant (Text "Osimhen")])]] False) env)
        ~?= "\\node (1) [decision, below of=0] {True ?};\n\\node (2) [statement, yshift=-0.5cm, xshift=-3cm, below left of=1] {print(\"Osimhen\")};\n"

    , "draw single if stmt"
        ~: (execWriter $ runStateT (drawStmts [If (VariableExp "True") [CallStmt (FunctionCall "print" [Constant (Text "Osimhen")])] Nothing] False) env)
        ~?= "\\node (1) [decision, below of=0] {True ?};\n\\node (2) [statement, yshift=-0.5cm, xshift=-1.5cm, below left of=1] {print(\"Osimhen\")};\n"

    , "draw nested if stmt"
        ~: (execWriter $ runStateT (drawStmts [If (VariableExp "True") [CallStmt (FunctionCall "print" [Constant (Text "Osimhen")])] (Just (ElseIf (VariableExp "False") [CallStmt (FunctionCall "print" [Constant (Text "Zirkzee")])] Nothing))] False) env)
        ~?= "\\node (1) [decision, below of=0] {True ?};\n\\node (2) [statement, yshift=-0.5cm, xshift=-1.5cm, below left of=1] {print(\"Osimhen\")};\n\\node (3) [decision, yshift=-0.5cm, xshift=1.5cm, below right of=1] {False ?};\n\\node (4) [statement, yshift=-0.5cm, xshift=-1.5cm, below left of=3] {print(\"Zirkzee\")};\n"

    , "draw if stmt with else"
        ~: (execWriter $ runStateT (drawStmts [If (VariableExp "True") [CallStmt (FunctionCall "print" [Constant (Text "Osimhen")])] (Just (ElseIf (VariableExp "False") [CallStmt (FunctionCall "print" [Constant (Text "Zirkzee")])] (Just (Else [CallStmt (FunctionCall "print" [Constant (Text "Berardi")])]))))] False) env)
        ~?= "\\node (1) [decision, below of=0] {True ?};\n\\node (2) [statement, yshift=-0.5cm, xshift=-1.5cm, below left of=1] {print(\"Osimhen\")};\n\\node (3) [decision, yshift=-0.5cm, xshift=1.5cm, below right of=1] {False ?};\n\\node (4) [statement, yshift=-0.5cm, xshift=-1.5cm, below left of=3] {print(\"Zirkzee\")};\n\\node (5) [statement, yshift=-0.5cm, xshift=1.5cm, below right of=3] {print(\"Berardi\")};\n"

    , "draw foreach stmt"
        ~: (execWriter $ runStateT (drawStmts [ForEach "t" (VariableExp "teams") [CallStmt (FunctionCall "print" [VariableExp "t"])]] False) env)
        ~?= "\\node (1) [decision, below of=0] {Iterated teams ?};\n\\node (2) [statement, yshift=-0.5cm, xshift=1.5cm, below right of=1] {t $\\gets$ next element in teams};\n\\node (3) [statement, below of=2] {print(t)};\n"

    , "draw positive for stmt"
        ~: (execWriter $ runStateT (drawStmts [For "t" (Constant (Number 0)) (Constant (Number 10)) [CallStmt (FunctionCall "print" [VariableExp "t"])]] False) env)
        ~?= "\\node (1) [statement, below of=0] {t $\\gets$ 0};\n\\node (2) [decision, below of=1] {t $<$ 10 ?};\n\\node (3) [statement, yshift=-0.5cm, xshift=1.5cm, below right of=2] {print(t)};\n\\node (4) [statement, xshift=3cm, below right of=3] {t $\\gets$ t + 1};\n"

    , "draw negative for stmt"
        ~: (execWriter $ runStateT (drawStmts [For "t" (Constant (Number 10)) (Constant (Number 0)) [CallStmt (FunctionCall "print" [VariableExp "t"])]] False) env)
        ~?= "\\node (1) [statement, below of=0] {t $\\gets$ 10};\n\\node (2) [decision, below of=1] {t $>$ 0 ?};\n\\node (3) [statement, yshift=-0.5cm, xshift=1.5cm, below right of=2] {print(t)};\n\\node (4) [statement, xshift=3cm, below right of=3] {t $\\gets$ t - 1};\n"

    , "draw hash stmt"
        ~: (execWriter $ runStateT (drawStmts [HashStmt (Return (Constant (Text "result")))] False) env)
        ~?= ""

    , "draw annotation stmt"
        ~: (execWriter $ runStateT (drawStmts [AnnotationStmt "return result!" [Return (Constant (Text "result"))]] False) env)
        ~?= "\\node (1) [statement, below of=0] {return result!};\n"

    , "draw return stmt"
        ~: (execWriter $ runStateT (drawStmts [Return (Constant (Text "result"))] False) env)
        ~?= "\\node (1) [startstop, below of=0] {\"result\"};\n"
    ]


testFunctionDecl :: Test
testFunctionDecl = TestList
    [ "draw simple function declaration"
        ~: (execWriter $ runStateT (drawFunctionDecl (FunctionDecl "f" [Argument "x" "int", Argument "y" "int"] [])) env)
        ~?= "\\node (0) [startstop] {f(x, y)};\n"

    , "draw function declaration without args"
        ~: (execWriter $ runStateT (drawFunctionDecl (FunctionDecl "f" [] [])) env)
        ~?= "\\node (0) [startstop] {f()};\n"

    , "draw function declaration with statement"
        ~: (execWriter $ runStateT (drawFunctionDecl (FunctionDecl "f" [] [Return (VariableExp "x")])) env)
        ~?= "\\node (0) [startstop] {f()};\n\\node (1) [startstop, below of=0] {x};\n"

    , "draw function declaration with multiple statements"
        ~: (execWriter $ runStateT (drawFunctionDecl (FunctionDecl "f" [] [Break, Return (VariableExp "x")])) env)
        ~?= "\\node (0) [startstop] {f()};\n\\node (1) [statement, below of=0] {break};\n\\node (2) [startstop, below of=1] {x};\n"
    ]