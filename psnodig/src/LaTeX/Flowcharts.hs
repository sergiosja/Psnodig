module LaTeX.Flowcharts (Stack(..), writeFlowchart) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate)
import Control.Monad.Writer
import Control.Monad.State
import Syntax

-- ha en currentId, og ha en belowId
-- vi henter alltid currentId fra feks `getNewId`, også belowid fra `getCoreId` ellerno!

type Edge = String
data Stack = Stack { edges :: [Edge], ids :: Int, core :: Int } -- endre navn fra ids. vi har jo bare 1 id om gangen
type Flowchart = StateT Stack (Writer String)

-- Helper functions

uniqueID :: Flowchart Int
uniqueID = do
    stack <- get
    let currentId = ids stack
    let updatedId = currentId + 1
    put $ stack { ids = updatedId }
    return updatedId

getCoreID :: Flowchart Int
getCoreID = do
    stack <- get
    return $ core stack

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

-- Values

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
        drawField (str, (StructVal fields')) =
            let fields'' = drawStruct fields'
            in str ++ ": (" ++ fields'' ++ ")"
        drawField (str, val) =
            let val' = drawValue val False
            in str ++ ": " ++ val'


-- Expressions

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


-- Statements

drawStmt :: Statement -> String -> String -> Flowchart ()
drawStmt (Assignment target value) currentId pos = do
    -- currentId <- uniqueID
    tell $ "\\node (" ++ currentId ++ ") [statement, " ++ pos ++ "] {" ++ drawAssignmentTarget target ++ " = " ++ drawAssignmentValue value ++ "};\n"
    -- addEdge (currentId - 1) currentId "--"




drawStmt (Loop expr stmts) currentId pos = do
    -- currentId <- uniqueID
    tell $ "\\node (" ++ currentId ++ ") [decision, " ++ pos ++ "] {" ++ drawExpr expr ++ " ?};\n"
    -- addEdge (currentId - 1) currentId "--"
    drawLoopStmts stmts (read currentId :: Int)
--    modify (\env -> env { ids = currentId })




-- drawStmt (If expr stmts maybeElse) pos = do
--     currentId <- uniqueID
--     (parentId, children) <- peekStack
--     addEdge currentId parentId children
--     tell $ "\\node (" ++ currentId ++ ") [decision, " ++ pos ++ parentId ++ "] {" ++ drawExpr expr ++ "};\n"

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

drawStmt (CallStmt functionCall) currentId pos = do
    -- currentId <- uniqueID
    tell $ "\\node (" ++ currentId ++ ") [statement, " ++ pos ++ "] {" ++ drawFunctionCall functionCall ++ "};\n"
    -- addEdge (currentId - 1) currentId "--"

drawStmt (Return expr) currentId pos = do
    -- currentId <- uniqueID
    tell $ "\\node (" ++ currentId ++ ") [startstop, " ++ pos ++ "] {" ++ drawExpr expr ++ "};\n"
    -- addEdge (currentId - 1) currentId "--"

drawStmt (HashStmt _) _ _ = return ()
drawStmt (AnnotationStmt str _) currentId pos = do
    -- currentId <- uniqueID
    tell $ "\\node (" ++ currentId ++ ") [statement, " ++ pos ++ "] {" ++ str ++ "};\n"
    -- addEdge (currentId - 1) currentId "--"

drawStmt Break _ _ = return () -- denne kan vel fikses greit? feks peke på neste
drawStmt Continue _ _ = return () -- og denne? feks peke på forrige skop

drawStmt _ _ _ = return ()


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

drawLoopStmts :: [Statement] -> Int -> Flowchart ()
drawLoopStmts stmts coreNodeId = do
    stack <- get
    put $ stack { core = coreNodeId }

    currentId <- uniqueID
    let parentId = show $ currentId - 1

    case length stmts of
        0 -> return ()
        1 -> do
            drawStmt (head stmts) (show currentId) ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ parentId)
            addEdge parentId (show currentId) "--"
            addEdge (show $ coreNodeId + 1) (show coreNodeId) "-|" -- med mindre main stmt (den før head stmts) er loop!
        n -> do
            drawStmt (head stmts) (show currentId) ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ parentId)
            drawStmts (tail stmts)
            addEdge parentId (show currentId) "--"
            -- mapM_ (\stmt -> drawStmt stmt) (tail stmts)
            addEdge (show $ coreNodeId + n) (show coreNodeId) "-|" -- med mindre main stmt (den før head stmts) er loop!

-- Functions

drawFunction :: Function -> Flowchart ()
drawFunction (Function name args stmts) = do
    tell $ "\\node (0) [startstop] {" ++ name ++ "(" ++ intercalateArgs args ++ ")};\n"
    drawStmts stmts

drawStmts :: [Statement] -> Flowchart ()
drawStmts [] = return ()
drawStmts (loop@(Loop _ _) : []) = do
    currentId <- uniqueID
    let parentId = show $ currentId - 1

    drawStmt loop (show currentId) ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ parentId)
    addEdge parentId (show currentId) "--"

drawStmts (loop@(Loop _ _) : x : xs) = do
    currentId <- uniqueID
    let parentId = show $ currentId - 1
    drawStmt loop (show currentId) ("below of=" ++ parentId)
    addEdge parentId (show currentId) "--"

    currentId' <- uniqueID
    -- let parentId' = show $ currentId' - 1
    coreId <- show <$> getCoreID
    drawStmt x (show currentId') ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ coreId)
    addEdge coreId (show currentId') "--"

    drawStmts xs

drawStmts (x:xs) = do
    currentId <- uniqueID
    let parentId = show $ currentId - 1

    drawStmt x (show currentId) ("below of=" ++ parentId)
    addEdge parentId (show currentId) "--"

    drawStmts xs

drawFunctionCall :: FunctionCall -> String
drawFunctionCall (FunctionCall name exprs) = name ++ "(" ++ intercalateExprs exprs ++ ")"


-- Edges

drawEdges :: Flowchart ()
drawEdges = do
    (Stack edges' _ _) <- get
    tell "\n" >> mapM_ tell (reverse edges')

addEdge :: String -> String -> String -> Flowchart ()
addEdge fromId toId direction = do
    let newEdge = "\\draw [edge] (" ++ fromId ++ ") " ++  direction ++ " (" ++ toId ++ ");\n"
    modify (\stack -> stack { edges = newEdge : (edges stack) })

-- addLoopEdges :: Int -> Flowchart ()
-- addLoopEdge [] = return ()
-- addLoopEdge (x : []) = do
--     (Stack _ currentId) <- get
--     addEdge -- fra x til "kjernen"
-- addLoopEdge x


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
