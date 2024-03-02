module LaTeX.Flowcharts (Stack(..), writeFlowchart) where

-- import qualified Data.Map as Map
import Control.Monad.Writer
import Control.Monad.State
import Syntax

type Scope = (String, [String])
data Stack = Stack { edges :: [Scope], ids :: Int }
type Flowchart = StateT Stack (Writer String)

-- Helper functions

uniqueID :: Flowchart String
uniqueID = do
    stack <- get
    let currentId = ids stack
    let updatedId = currentId + 1
    put $ stack { ids = updatedId }
    return $ show updatedId

peekStack :: Flowchart Scope
peekStack = do
    Stack existingEdges _ <- get
    case existingEdges of
        [] -> return ("",[])
        entry : _ -> return entry

updateStack :: String -> String -> [String] -> Flowchart ()
updateStack current parent children =
    modify (\stack -> stack { edges = (current, []) : (parent, current : children) : tail (edges stack) })


-- må ha en stack for å vite hva som er på!

drawFunction :: Function -> Flowchart ()
drawFunction (Function name args stmts) = do
    tell $ "\\node (start) [startstop] {" ++ name
    drawArgs args
    mapM_ drawStmt stmts

drawArgs :: [Argument] -> Flowchart ()
drawArgs [] = tell "()};"
drawArgs [x] =
    tell $ " ( " ++ (fstArg x) ++ " )};\n"
drawArgs args = do
    tell $ "( "
    mapM_ (\x -> tell $ (fstArg x) ++ ", ") (init args)
    tell $ (show $ fstArg $ last args) ++ ")};\n"

fstArg :: Argument -> String
fstArg (Argument x _) = x


-- Drawing values

drawValue :: Value -> String
drawValue Nil = "empty"
drawValue (Boolean True) = "true"
drawValue (Boolean False) = "false"
drawValue (Number n) = show n
drawValue (Text t) = t
-- List [Expression]
-- HashSet (Set.Set Expression)
-- HashMap (Map.Map Expression Expression)
-- StructVal [(String, Value)]
drawValue v = show v


-- Drawing expressions

drawExpr :: Expression -> String
drawExpr (Constant v) = drawValue v
drawExpr (VariableExp v) = v
drawExpr (BinaryExp op expr1 expr2) = drawExpr expr1 ++ drawOp op ++ drawExpr expr2
drawExpr (ListIndex listName exprs) = listName ++ drawIndexExprs exprs
drawExpr (CallExp functionCall) = drawFunctionCall functionCall
drawExpr (Not expr) = "not " ++ drawExpr expr
drawExpr (StructExpr (Struct structName exprs)) = structName ++ "(" ++ drawExprsInParentheses exprs
drawExpr (StructFieldExp (StructField expr1 expr2)) =
    drawExpr expr1 ++ "." ++ drawExpr expr2
drawExpr _ = "" -- må fjerne LocalStructField fra Expressions på et tidspunkt


drawOp :: Operator -> String
drawOp Plus = " + "
drawOp Minus = " - "
drawOp Times = " $\\cdot$ "
drawOp Division = " / "

drawOp LessThan = " $<$ "
drawOp LessThanEqual = " $\\leq$ "
drawOp GreaterThan = " $>$ "
drawOp GreaterThanEqual = " $\\geq$ "
drawOp Equal = " $=$ "
drawOp NotEqual = " $\\neq$$ "

drawOp And = " $\\land$ "
drawOp Or = " $\\lor$ "
drawOp Modulo = " $%$ "

drawIndexExprs :: [Expression] -> String
drawIndexExprs [] = ""
drawIndexExprs (x:xs) = "[" ++ drawExpr x ++ "]" ++ drawIndexExprs xs


-- Drawing statements

drawStmt :: Statement -> Flowchart ()
drawStmt (Assignment target value) = do
    currentId <- uniqueID
    (parentId, children) <- peekStack
    -- get last entry from stack
    tell $ "\\node (" ++ currentId ++ ") [statement, below of=" ++ parentId ++ "] {" ++ drawAssignmentTarget target ++ " = " ++ drawAssignmentValue value ++ "};\n"
    updateStack currentId parentId children

-- drawStmt (Loop expr stmts) =

drawStmt (If expr stmts maybeElse) = do
    currentId <- uniqueID
    (parentId, children) <- peekStack
    updateStack currentId parentId children
    tell $ "\\node (" ++ currentId ++ ") [decision, below of=" ++ parentId ++ "] {" ++

--     tell $ "\\node (" ++ ??? ++ ") [decision, below of=" ++ ??? ++ ", xshift=2cm] {" ++ drawExpr expr ++ "};\n"
--     mapM_ drawStmt stmts

-- drawStmt (ForEach str expr stmts) =
-- drawStmt (For str expr1 expr2 stmts) =
drawStmt (CallStmt functionCall) = tell $ drawFunctionCall functionCall

drawStmt (Return expr) = do
    currentId <- uniqueID
    (parentId, children) <- peekStack
    tell $ "\\node (" ++ currentId ++ ") [startstop, below of=" ++ parentId ++ "] {" ++ (drawExpr expr) ++ "};\n"
    updateStack currentId parentId children

drawStmt (HashStmt _) = return ()
drawStmt (AnnotationStmt str _) = do
    currentId <- uniqueID
    (parentId, children) <- peekStack
    tell $ "\\node (" ++ currentId ++ ") [statement, below of=" ++ parentId ++ "] {" ++ str ++ "};\n"
    updateStack currentId parentId children

drawStmt Break = return () -- denne kan vel fikses greit? feks peke på neste
drawStmt Continue = return () -- og denne? feks peke på forrige skop

drawStmt _ = return ()


drawAssignmentTarget :: AssignmentTarget -> String
drawAssignmentTarget (VariableTarget v) = v
drawAssignmentTarget (ListIndexTarget v indexes) =
    v ++ (show $ map (\x -> "[" ++ drawExpr x ++ "]") indexes)
drawAssignmentTarget (StructFieldTarget (StructField x y)) =
    drawExpr x ++ "." ++ drawExpr y

drawAssignmentValue :: AssignmentValue -> String
drawAssignmentValue (ExpressionValue expr) = drawExpr expr
drawAssignmentValue (StructValue (Struct name exprs)) = name ++
    case length exprs of
        0 -> "()"
        1 -> "(" ++ drawExpr (head exprs) ++ ")"
        _ -> "(" ++ (show $ map (\x -> drawExpr x ++ ", ") (init exprs)) ++ (drawExpr $ last exprs) ++ ")"


-- Functions

drawFunctionCall :: FunctionCall -> String
drawFunctionCall (FunctionCall name exprs) = name ++ drawExprsInParentheses exprs -- trenger en counter på name her!


drawExprsInParentheses :: [Expression] -> String
drawExprsInParentheses [] = ")"
drawExprsInParentheses (x:[]) = drawExpr x ++ ")"
drawExprsInParentheses (x:xs) = drawExpr x ++ ", " ++ drawExprsInParentheses xs

-- Config

-- hadde vært kult om man kunne sende med egne sånne? feks gjennom en fil, også er brukeren selv ansvarlig for at alt er riktig
-- istedenfor å være bundet til startstop, io osv. å legge inn en egen \tikzstyle{sergey_custom} = [rectangle, minimum width=15cm, ..]
constantConfig :: Flowchart ()
constantConfig = do
    tell "\\documentclass{article}\n\\usepackage{tikz}\n\\usetikzlibrary{shapes.geometric, arrows}\n\n"
    -- tell "\\tikzstyle{startstop} = [rectangle, rounded corners,\nminimum width=3cm,\nminimum height=1cm,\ntext centered,\ndraw=black,\nfill=red!30]\n\n"
    tell "\\tikzstyle{startstop} = [rectangle, rounded corners, minimum width=2cm, minimum height=1cm, text centered, draw=black, text=white, fill=black!80]\n"
    -- tell "\\tikzstyle{statement} = [rectangle,\nminimum width=3cm,\nminimum height=1cm,\ntext centered,\ntext width=3cm,\ndraw=black,\nfill=orange!30]\n\n"
    tell "\\tikzstyle{statement} = [rectangle, minimum width=4cm, minimum height=1cm, text centered, draw=black, fill=blue!20]\n"
    -- tell "\\tikzstyle{decision} = [diamond,\nminimum width=3cm,\nminimum height=1cm,\ntext centered,\ndraw=black,\nfill=green!30]\n\n"
    tell "\\tikzstyle{decision} = [diamond, text centered, draw=black, fill=yellow!30]\n"
    tell "\\tikzstyle{edge} = [thick, ->, >=stealth]\n\n"
    tell "\\begin{document}\n\\begin{tikzpicture}[node distance=2cm]\n\n"

writeFlowchart :: Program -> Flowchart ()
writeFlowchart (Program _ funcs _) = do
    constantConfig
    drawFunction $ head funcs
    tell "\n\\end{tikzpicture}\n\\end{document}"
