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

countBranches :: (Maybe Else) -> Int
countBranches Nothing = 0
countBranches (Just (Else _)) = 1
countBranches (Just (ElseIf _ _ branch)) = 1 + countBranches branch


-- Drawing functions

drawFunction :: Function -> Flowchart ()
drawFunction (Function name args stmts) = do
    tell $ "\\node (0) [startstop] {" ++ name ++ "(" ++ drawArgs args ++ ")};\n"
    mapM_ (\s -> drawStmt s "below of=") stmts

drawArgs :: [Argument] -> String
drawArgs [] = ""
drawArgs ((Argument x _) : []) = x
drawArgs ((Argument x _) : xs) = x ++ ", " ++ drawArgs xs


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

drawStmt :: Statement -> String -> Flowchart ()
drawStmt (Assignment target value) pos = do
    currentId <- uniqueID
    (parentId, children) <- peekStack
    tell $ "\\node (" ++ currentId ++ ") [statement, " ++ pos ++ parentId ++ "] {" ++ drawAssignmentTarget target ++ " = " ++ drawAssignmentValue value ++ "};\n"
    updateStack currentId parentId children

-- drawStmt (Loop expr stmts) =

drawStmt (If expr stmts maybeElse) pos = do
    currentId <- uniqueID
    (parentId, children) <- peekStack
    updateStack currentId parentId children
    tell $ "\\node (" ++ currentId ++ ") [decision, " ++ pos ++ parentId ++ "] {" ++ drawExpr expr ++ "};\n"

    -- Find number of branches
    -- let numberOfBranches = countBranches maybeElse
    -- case numberOfBranches of
    --     0 -> do
            -- drawStmt _ "below left of="
            -- drawStmt _ "below right of="

            {- ### 2 scenarios:

            1. No `return` in `stmts`
                - Edge from currentId to first stmt in `smts`
                - Edge from currentId to next stmt after this `If`
                - Edge from last stmt in `stmts` to the next stmt after this `If`

            2. A `return` in `stmts`
                - Edge from currentId to first stmt in `stmts`
                - Edge from currentId to next stmt after this `If`
            -}
        -- n -> do
            {- ### 1 scenario?

            - Calculate xshift- and yshift values depending on `n`
            - Edge from currentId to first stmt in `stmts`
            - Edge from currentId to the first else-branch
            - Edge from all branches without `return` to the next stmt after this `If`
            -}

        -- ### Also
        -- If all branches have a return, we can technically finish,
        -- as next stmt will be unreahable

-- drawStmt (ForEach str expr stmts) =
-- drawStmt (For str expr1 expr2 stmts) =

drawStmt (CallStmt functionCall) pos = do
    currentId <- uniqueID
    (parentId, children) <- peekStack
    tell $ "\\node (" ++ currentId ++ ") [statement, " ++ pos ++ parentId ++ "] {" ++ drawFunctionCall functionCall ++ "};\n"
    updateStack currentId parentId children

drawStmt (Return expr) pos = do
    currentId <- uniqueID
    (parentId, children) <- peekStack
    tell $ "\\node (" ++ currentId ++ ") [startstop, " ++ pos ++ parentId ++ "] {" ++ drawExpr expr ++ "};\n"
    updateStack currentId parentId children

drawStmt (HashStmt _) _ = return ()
drawStmt (AnnotationStmt str _) pos = do
    currentId <- uniqueID
    (parentId, children) <- peekStack
    tell $ "\\node (" ++ currentId ++ ") [statement, " ++ pos ++ parentId ++ "] {" ++ str ++ "};\n"
    updateStack currentId parentId children

drawStmt Break _ = return () -- denne kan vel fikses greit? feks peke på neste
drawStmt Continue _ = return () -- og denne? feks peke på forrige skop

drawStmt _ _ = return ()


drawAssignmentTarget :: AssignmentTarget -> String
drawAssignmentTarget (VariableTarget v) = v
drawAssignmentTarget (ListIndexTarget v indexes) =
    v ++ (show $ map (\x -> "[" ++ drawExpr x ++ "]") indexes)
drawAssignmentTarget (StructFieldTarget (StructField x y)) =
    drawExpr x ++ "." ++ drawExpr y

drawAssignmentValue :: AssignmentValue -> String
drawAssignmentValue (ExpressionValue expr) = drawExpr expr
drawAssignmentValue (StructValue (Struct name exprs)) = name ++ "(" ++ drawExprsInParentheses exprs ++ ")"

-- drawElse :: Else -> ..
-- drawElse maybeElse


-- Functions

drawFunctionCall :: FunctionCall -> String
drawFunctionCall (FunctionCall name exprs) = name ++ "(" ++ drawExprsInParentheses exprs ++ ")" -- trenger en counter på name her!

drawExprsInParentheses :: [Expression] -> String
drawExprsInParentheses [] = ""
drawExprsInParentheses (x:[]) = drawExpr x
drawExprsInParentheses (x:xs) = drawExpr x ++ ", " ++ drawExprsInParentheses xs


-- Edges

drawEdges :: Flowchart ()
drawEdges = do
    (Stack edges' _) <- get
    tell "\n" >> mapM_ drawEdge edges'

drawEdge :: Scope -> Flowchart ()
drawEdge (parent, children) =
    mapM_ (\child -> tell $ "\\draw [edge] (" ++ parent ++ ") -- (" ++ child ++ ");\n") children


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
    drawEdges
    tell "\n\\end{tikzpicture}\n\\end{document}"
