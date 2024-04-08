module LaTeX.Flowcharts (Stack(..), writeFlowchart) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate)
import Control.Monad.Writer
import Control.Monad.State
import Syntax

type Edge = String
data Stack = Stack { edges :: [Edge], previousId :: Int, coreId :: Int }
type Flowchart = StateT Stack (Writer String)

-- Helper functions

generateNewId :: Flowchart Int
generateNewId = do
    stack <- get
    let updatedId = previousId stack + 1
    put $ stack { previousId = updatedId }
    return updatedId

getCoreId :: Flowchart Int
getCoreId = do
    stack <- get
    return $ coreId stack

updateCoreId :: Int -> Flowchart ()
updateCoreId newCoreId = do
    stack <- get
    put $ stack { coreId = newCoreId }

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

toInt :: Expression -> Int
toInt x = read (drawExpr x) :: Int


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

drawStmts :: [Statement] -> Flowchart ()
drawStmts [] = return ()
drawStmts (loop@(Loop _ _) : []) = initiateLoop loop
drawStmts (loop@(Loop _ _) : x : xs) = do
    initiateLoop loop

    -- Right side of loop
    currentId' <- show <$> generateNewId
    currentCoreId <- show <$> getCoreId
    drawStmt x currentId' ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ currentCoreId)
    addEdge currentCoreId currentId' "-- node[anchor=west, yshift=0.1cm]{true}"
    drawStmts xs

drawStmts (forEach@(ForEach _ _ _) : []) = initiateLoop forEach
drawStmts (forEach@(ForEach _ _ _) : x : xs) = do
    initiateLoop forEach

    -- Left side of loop
    currentId' <- show <$> generateNewId
    currentCoreId <- show <$> getCoreId
    drawStmt x currentId' ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ currentCoreId)
    addEdge currentCoreId currentId' "-- node[anchor=east, yshift=0.1cm]{true}"
    drawStmts xs

drawStmts (for@(For _ _ _ _) : []) = initiateLoop for
drawStmts (for@(For _ _ _ _) : x : xs) = do
    initiateLoop for

    -- Left side of loop
    currentId' <- show <$> generateNewId
    currentCoreId <- show <$> getCoreId
    drawStmt x currentId' ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ currentCoreId)
    addEdge currentCoreId currentId' "-- node[anchor=east, yshift=0.1cm]{false}"
    drawStmts xs

drawStmts (x:xs) = do
    currentId <- show <$> generateNewId
    let parentId = show $ (read currentId :: Int) - 1
    drawStmt x currentId ("below of=" ++ parentId)
    addEdge parentId currentId "--"
    drawStmts xs

drawStmt :: Statement -> String -> String -> Flowchart ()
drawStmt (Assignment target value) currentId pos =
    drawStatementNode currentId pos (drawAssignmentTarget target ++ " = " ++ drawAssignmentValue value)

drawStmt (Loop expr stmts) currentId pos = do
    drawDecisionNode currentId pos (drawExpr expr)
    drawLoopStmts stmts (read currentId :: Int)

-- drawStmt (If expr stmts maybeElse) pos = do
--     currentId <- generateNewId
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

drawStmt (ForEach identifier expr stmts) currentId pos = do
    drawDecisionNode currentId pos ("Iterated " ++ drawExpr expr)
    drawForEachStmts stmts identifier (read currentId :: Int)

drawStmt (For identifier expr1 expr2 stmts) currentId pos = do
    drawStatementNode currentId pos (identifier ++ " = " ++ drawExpr expr1)
    let op = if toInt expr1 <= toInt expr2 then " $<$ " else " $>$ "
    decisionId <- show <$> generateNewId
    drawDecisionNode decisionId ("below of=" ++ currentId) (identifier ++ op ++ drawExpr expr2)
    addEdge currentId decisionId "--"

    let crement = if toInt expr1 <= toInt expr2
        then identifier ++ " = " ++ identifier ++ " + 1"
        else identifier ++ " = " ++ identifier ++ " - 1"
    drawForStmts stmts (read decisionId :: Int) crement

drawStmt (CallStmt functionCall) currentId pos =
    drawStatementNode currentId pos (drawFunctionCall functionCall)

drawStmt (Return expr) currentId pos = do
    drawStartstopNode currentId pos (drawExpr expr)

drawStmt (HashStmt _) _ _ = return ()
drawStmt (AnnotationStmt text _) currentId pos =
    drawStatementNode currentId pos text

drawStmt Break _ _ = return ()
drawStmt Continue _ _ = return ()
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

-- Loop helpers

initiateLoop :: Statement -> Flowchart ()
initiateLoop loop = do
    currentId <- show <$> generateNewId
    let parentId = show $ (read currentId :: Int) - 1
    drawStmt loop currentId ("below of=" ++ parentId)
    addEdge parentId currentId "--"

drawLoopStmts :: [Statement] -> Int -> Flowchart ()
drawLoopStmts stmts coreNodeId = do
    updateCoreId coreNodeId
    currentId <- show <$> generateNewId
    let parentId = show $ (read currentId :: Int) - 1
    case length stmts of
        0 -> return ()
        1 -> do
            drawStmt (head stmts) currentId ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ parentId)
            addEdge parentId currentId "-- node[anchor=east, yshift=0.1cm]{false}"
            addEdge (show $ coreNodeId + 1) (show coreNodeId) "-|" -- med mindre main stmt (den før head stmts) er loop!
        n -> do
            drawStmt (head stmts) currentId ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ parentId)
            addEdge parentId currentId "-- node[anchor=east, yshift=0.1cm]{false}"
            drawStmts (tail stmts)
            addEdge (show $ coreNodeId + n) (show coreNodeId) "-|" -- med mindre main stmt (den før head stmts) er loop!

drawForEachStmts :: [Statement] -> String -> Int -> Flowchart ()
drawForEachStmts stmts identifier coreNodeId = do
    updateCoreId coreNodeId
    currentId <- show <$> generateNewId
    let parentId = show $ (read currentId :: Int) - 1
    drawStatementNode currentId ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ parentId) (identifier ++ " = next element") -- in collection
    addEdge parentId currentId "-- node[anchor=west, yshift=0.1cm]{false}"
    case length stmts of
        0 -> return ()
        n -> do
            drawStmts stmts
            addEdge (show $ coreNodeId + n + 1) (show coreNodeId) "-|"

drawForStmts :: [Statement] -> Int -> String -> Flowchart ()
drawForStmts stmts coreNodeId crement = do
    updateCoreId coreNodeId
    currentId <- show <$> generateNewId
    let parentId = show $ (read currentId :: Int) - 1
    case length stmts of
        0 -> return ()
        1 -> do
            drawStmt (head stmts) currentId ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ parentId)
            addEdge parentId currentId "-- node[anchor=west, yshift=0.1cm]{true}"

            crementId <- show <$> generateNewId
            tell $ "\\node (" ++ crementId ++ ") [statement, xshift=3cm, below right of=" ++ currentId ++ "] {" ++ crement ++ "};\n"
            addEdge currentId crementId "|-"
            addEdge crementId (show coreNodeId) "|-"
        _ -> do
            drawStmt (head stmts) currentId ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ parentId)
            addEdge parentId currentId "-- node[anchor=west, yshift=0.1cm]{true}"
            drawStmts (tail stmts)

            crementId <- show <$> generateNewId
            let crementParentId = show $ (read crementId :: Int) - 1
            tell $ "\\node (" ++ crementId ++ ") [statement, xshift=3cm, below right of=" ++ crementParentId ++ "] {" ++ crement ++ "};\n"
            addEdge crementParentId crementId "|-"
            addEdge crementId (show coreNodeId) "|-"


-- Functions

drawFunction :: Function -> Flowchart ()
drawFunction (Function name args stmts) = do
    tell $ "\\node (0) [startstop] {" ++ name ++ "(" ++ intercalateArgs args ++ ")};\n"
    drawStmts stmts

drawFunctionCall :: FunctionCall -> String
drawFunctionCall (FunctionCall name exprs) = name ++ "(" ++ intercalateExprs exprs ++ ")"


-- Nodes

drawStartstopNode :: String -> String -> String -> Flowchart ()
drawStartstopNode currentId pos text =
    tell $ "\\node (" ++ currentId ++ ") [startstop, " ++ pos ++ "] {" ++ text ++ "};\n"

drawStatementNode :: String -> String -> String -> Flowchart ()
drawStatementNode currentId pos text =
    tell $ "\\node (" ++ currentId ++ ") [statement, " ++ pos ++ "] {" ++ text ++ "};\n"

drawDecisionNode :: String -> String -> String -> Flowchart ()
drawDecisionNode currentId pos text =
    tell $ "\\node (" ++ currentId ++ ") [decision, " ++ pos ++ "] {" ++ text ++ " ?};\n"


-- Edges

drawEdges :: Flowchart ()
drawEdges = do
    (Stack edges' _ _) <- get
    tell "\n" >> mapM_ tell (reverse edges')

addEdge :: String -> String -> String -> Flowchart ()
addEdge fromId toId direction = do
    let newEdge = "\\draw [edge] (" ++ fromId ++ ") " ++  direction ++ " (" ++ toId ++ ");\n"
    modify (\stack -> stack { edges = newEdge : (edges stack) })

-- Entry point

-- hadde vært kult om man kunne sende med egne sånne? feks gjennom en fil, også er brukeren selv ansvarlig for at alt er riktig
-- istedenfor å være bundet til startstop, io osv. å legge inn en egen \tikzstyle{sergey_custom} = [rectangle, minimum width=15cm, ..]
constantConfig :: Flowchart ()
constantConfig = do
    tell "\\documentclass{article}\n\\usepackage{tikz}\n\\usetikzlibrary{shapes.geometric, arrows}\n\n"
    tell "\\tikzstyle{startstop} = [rectangle, rounded corners, minimum width=2cm, minimum height=1cm, text centered, draw=black, text=white, fill=black!80]\n"
    tell "\\tikzstyle{statement} = [rectangle, minimum width=4cm, minimum height=1cm, text centered, draw=black, fill=blue!20]\n"
    tell "\\tikzstyle{decision} = [ellipse, minimum height=1cm, text centered, draw=black, fill=yellow!30]\n"
    tell "\\tikzstyle{edge} = [thick, ->, >=stealth]\n\n"
    tell "\\begin{document}\n\\begin{tikzpicture}[node distance=2cm]\n\n"

writeFlowchart :: Program -> Flowchart ()
writeFlowchart (Program _ funcs _) = do
    constantConfig
    drawFunction $ head funcs
    drawEdges
    tell "\n\\end{tikzpicture}\n\\end{document}"
