module LaTeX.Flowcharts (Stack(..), writeFlowchart) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate)
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
        [] -> return ("", [])
        entry : _ -> return entry

updateStack :: String -> String -> [String] -> Flowchart ()
updateStack current parent children =
    modify (\stack -> stack { edges = (current, []) : (parent, current : children) : tail (edges stack) })

countBranches :: (Maybe Else) -> Int
countBranches Nothing = 0
countBranches (Just (Else _)) = 1
countBranches (Just (ElseIf _ _ branch)) = 1 + countBranches branch

intercalateExprs :: [Expression] -> String
intercalateExprs exprs =
    let exprs' = map drawExpr exprs
    in intercalate ", " exprs'

intercalateArgs :: [Argument] -> String
intercalateArgs args =
    let args' = map drawArg args
    in intercalate ", " args'
    where
        drawArg :: Argument -> String
        drawArg (Argument x _) = x

drawLoopStmts :: [Statement] -> Flowchart ()
drawLoopStmts stmts =
    case length stmts of
        0 -> return ()
        1 -> drawStmt (head stmts) "yshift=-0.5cm, xshift=-1.5cm, below left of="
        _ -> do
            drawStmt (head stmts) "yshift=-0.5cm, xshift=-1.5cm, below left of="
            mapM_ (\stmt -> drawStmt stmt "below of=") (tail stmts)

-- Drawing values

drawValue :: Value -> Bool -> String
drawValue Nil _ = "Nil"
drawValue (Boolean b) _ = show b
drawValue (Number n) _ = show n
drawValue (Decimal d) _ = show d
drawValue (Text t) b = if b then '\"' : t ++ "\"" else t
drawValue (List l) _ = drawIterable "[" l "]"
drawValue (HashSet hs) _ = drawIterable "(" (Set.toList hs) ")"
drawValue (HashMap hm) _ = drawHMap $ Map.toList hm
drawValue (StructVal sf) _ = drawStruct sf

drawIterable :: String -> [Expression] -> String -> String
drawIterable l it r =
    let vals = map drawExpr it
    in l ++ intercalate ", " vals ++ r

drawHMap :: [(Expression, Expression)] -> String
drawHMap pairs =
    let vals = map drawPair pairs
        vals' = map (\(x, y) -> x ++ ": " ++ y) vals
    in "{" ++ intercalate ", " vals' ++ "}"
    where
        drawPair :: (Expression, Expression) -> (String, String)
        drawPair (x, y) =
            let x' = drawExpr x
                y' = drawExpr y
            in (x', y')

drawStruct :: [(String, Value)] -> String
drawStruct fields =
    let fields' = map drawField fields
    in intercalate ", " fields'
    where
        drawField :: (String, Value) -> String
        drawField (str, (StructVal fields)) =
            let fields' = drawStruct fields
            in str ++ ": (" ++ fields' ++ ")"
        drawField (str, val) =
            let val' = drawValue val False
            in str ++ ": " ++ val'


-- Drawing expressions

drawExpr :: Expression -> String
drawExpr (Constant v) = drawValue v False
drawExpr (VariableExp v) = v
drawExpr (BinaryExp op expr1 expr2) = drawExpr expr1 ++ drawOp op ++ drawExpr expr2
drawExpr (ListIndex listName exprs) = listName ++ drawIndexExprs exprs
drawExpr (CallExp functionCall) = drawFunctionCall functionCall
drawExpr (Not expr) = "not " ++ drawExpr expr
drawExpr (StructExpr (Struct structName exprs)) = structName ++ "(" ++ intercalateExprs exprs
drawExpr (StructFieldExp (StructField expr1 expr2)) =
    drawExpr expr1 ++ "." ++ drawExpr expr2


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




drawStmt (Loop expr stmts) pos = do
    currentId <- uniqueID
    (parentId, children) <- peekStack
    tell $ "\\node (" ++ currentId ++ ") [decision, " ++ pos ++ parentId ++ "] {" ++ drawExpr expr ++ " ?};\n"
    updateStack currentId parentId children

    drawLoopStmts stmts

    updateStack currentId parentId []




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
drawAssignmentValue (StructValue (Struct name exprs)) = name ++ "(" ++ intercalateExprs exprs ++ ")"

-- drawElse :: Else -> ..
-- drawElse maybeElse


-- Functions

drawFunction :: Function -> Flowchart ()
drawFunction (Function name args stmts) = do
    tell $ "\\node (0) [startstop] {" ++ name ++ "(" ++ intercalateArgs args ++ ")};\n"
    drawFuncStmts stmts
    -- mapM_ (\s -> drawStmt s "below of=") stmts

drawFuncStmts :: [Statement] -> Flowchart ()
drawFuncStmts [] = return ()

drawFuncStmts (loop@(Loop _ _) : []) = do
    drawStmt loop "below of="
drawFuncStmts (loop@(Loop _ _) : x : xs) = do
    drawStmt loop "below of="
    drawStmt x "yshift=-0.5cm, xshift=1.5cm, below right of="
    drawFuncStmts xs

drawFuncStmts (x:xs) = do
    drawStmt x "below of="
    drawFuncStmts xs

drawFunctionCall :: FunctionCall -> String
drawFunctionCall (FunctionCall name exprs) = name ++ "(" ++ intercalateExprs exprs ++ ")"

-- Edges

drawEdges :: Flowchart ()
drawEdges = do
    (Stack edges' _) <- get
    tell "\n" >> mapM_ drawEdge edges'

drawEdge :: Scope -> Flowchart ()
drawEdge (parent, children) =
    mapM_ (\child -> tell $ "\\draw [edge] (" ++ parent ++ ") -- (" ++ child ++ ");\n") children



{-

Kantene til en while-loop:
    left ->
        \draw [edge] (loop-kjernen) -- node[anchor=west] {true} (første loop-stmt);

    right ->
        \draw [edge] (loop-kjernen) -- node[anchor=east] {false} (resten av statementene);


Fra siste stmt:
    \draw [edge] (siste stmt) -| (loop-kjernen);


Tanke: en egen drawLoopEdges-funksjon. Det er nemlig ikke farlig hvilken rekkefølge de kommer i

OOOK det går faktisk an å ha dem etter hverandre osv

-}


-- Entry point

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
