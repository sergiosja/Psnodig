module LaTeX.Flowcharts (Stack(..), writeFlowchart) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate)
import Control.Monad.Writer
import Control.Monad.State
import Syntax

data Stack = Stack
    { edges :: [String]
    , lastId :: Int
    , coreIds :: [Int]
    , activeBranches :: [String]
    }

type Flowchart = StateT Stack (Writer String)

-- Helper functions

getLastId :: Flowchart String
getLastId = do
    (Stack _ currentId _ _) <- get
    return . show $ currentId

getNewId :: Flowchart Int
getNewId = do
    stack <- get
    let updatedId = lastId stack + 1
    put $ stack { lastId = updatedId }
    return updatedId

getNextEdge :: Flowchart (String, String)
getNextEdge = do
    currentId <- getNewId
    let parentId = currentId - 1
    return (show currentId, show parentId)

setCoreId :: Int -> Flowchart ()
setCoreId newCoreId = do
    currStack@(Stack { coreIds = ids }) <- get
    put currStack { coreIds = (newCoreId : ids) }

popCoreId :: Flowchart Int
popCoreId = do
    currStack@(Stack { coreIds = ids }) <- get
    case ids of
        [] -> error "Stack underflow: Popping empty coreId stack"
        (currentCoreId : rest) -> do
            put currStack { coreIds = rest }
            return currentCoreId

addActiveBranch :: String -> Flowchart ()
addActiveBranch newBranch = do
    currStack@(Stack { activeBranches = branches }) <- get
    put currStack { activeBranches = (newBranch : branches) }

emptyActiveBranches :: Flowchart ()
emptyActiveBranches = do
    currStack@(Stack { activeBranches = branches }) <- get
    put currStack { activeBranches = [] }

checkActiveBranch :: Statement -> String -> Flowchart ()
checkActiveBranch stmt branchId =
    case stmt of
        Loop _ _ -> return ()
        Return _ -> return ()
        If _ _ _ -> return ()
        ForEach _ _ _ -> return ()
        _ -> addActiveBranch branchId

getActiveBranches :: Flowchart [String]
getActiveBranches = do
    (Stack _ _ _ branches) <- get
    return branches

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

initiateStmt :: Statement -> Flowchart ()
initiateStmt stmt = do
    (currentId, parentId) <- getNextEdge
    drawStmt stmt currentId ("below of=" ++ parentId)
    addEdge parentId currentId "--"

drawStmts :: [Statement] -> Flowchart ()
drawStmts [] = return ()
drawStmts (stmt@(If _ _ maybeElse) : x : xs) = do

    -- Left side (true)
    initiateStmt stmt

    -- Right side (false)
    case maybeElse of
        Just (ElseIf expr stmts maybeElse') -> do
            currentId <- show <$> getNewId
            currentCoreId <- show <$> popCoreId
            drawStmt (If expr stmts maybeElse') currentId ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ currentCoreId)
            addEdge currentCoreId currentId "-- node[anchor=west, yshift=0.1cm]{false}"
            drawStmts (x:xs)

        Just (Else stmts) -> do
            currentId <- show <$> getNewId
            currentCoreId <- show <$> popCoreId
            drawStmt (head stmts) currentId ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ currentCoreId)
            addEdge currentCoreId currentId "-- node[anchor=west, yshift=0.1cm]{false}"
            drawStmts (tail stmts)

            (currentId, parentId) <- getNextEdge
            drawStmt x currentId ("below of=" ++ parentId)
            addEdgeFromActiveBranches
            drawStmts xs
            -- herfra laget vi kanter fra alle active branches til x, også fortsetter xs etter den!
            -- drawStmts xs

        Nothing -> do
            currentId <- show <$> getNewId
            currentCoreId <- show <$> popCoreId
            drawStmt x currentId ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ currentCoreId)
            addEdge currentCoreId currentId "-- node[anchor=west, yshift=0.1cm]{false}"
            drawStmts xs

drawStmts (cond@(If _ _ _) : []) = return ()






drawStmts (stmt@(Loop _ _) : []) = initiateStmt stmt >> void popCoreId
drawStmts (stmt@(Loop _ _) : x : xs) = do
    initiateStmt stmt

    -- Right side of loop
    currentId <- show <$> getNewId
    currentCoreId <- show <$> popCoreId
    drawStmt x currentId ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ currentCoreId)
    addEdge currentCoreId currentId "-- node[anchor=west, yshift=0.1cm]{false}"
    drawStmts xs

drawStmts (stmt@(ForEach _ _ _) : []) = initiateStmt stmt >> void popCoreId
drawStmts (stmt@(ForEach _ _ _) : x : xs) = do
    initiateStmt stmt

    -- Left side of loop
    currentId <- show <$> getNewId
    currentCoreId <- show <$> popCoreId
    drawStmt x currentId ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ currentCoreId)
    addEdge currentCoreId currentId "-- node[anchor=east, yshift=0.1cm]{true}"
    drawStmts xs

drawStmts (stmt@(For _ _ _ _) : []) = initiateStmt stmt >> void popCoreId
drawStmts (stmt@(For _ _ _ _) : x : xs) = do
    initiateStmt stmt

    -- Left side of loop
    currentId <- show <$> getNewId
    currentCoreId <- show <$> popCoreId
    drawStmt x currentId ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ currentCoreId)
    addEdge currentCoreId currentId "-- node[anchor=east, yshift=0.1cm]{false}"
    drawStmts xs

drawStmts (stmt : stmts) = do
    initiateStmt stmt
    drawStmts stmts


drawStmt :: Statement -> String -> String -> Flowchart ()
drawStmt (Assignment target value) currentId pos =
    drawStatementNode currentId pos (drawAssignmentTarget target ++ " $\\gets$ " ++ drawAssignmentValue value)

drawStmt (Loop expr stmts) currentId pos = do
    drawDecisionNode currentId pos (drawExpr expr)
    drawLoopStmts stmts (read currentId :: Int)

drawStmt (If expr stmts maybeElse) currentId pos = do
    drawDecisionNode currentId pos (drawExpr expr)
    setCoreId (read currentId :: Int)

    -- Left side
    headId <- show <$> getNewId
    drawStmt (head stmts) headId ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ currentId)
    addEdge currentId headId "-- node[anchor=east, yshift=0.1cm]{true}"
    drawIfStmts (tail stmts)

drawStmt (ForEach identifier expr stmts) currentId pos = do
    let collection = drawExpr expr
    drawDecisionNode currentId pos ("Iterated " ++ collection)
    drawForEachStmts stmts identifier collection (read currentId :: Int)

drawStmt (For identifier expr1 expr2 stmts) currentId pos = do
    drawStatementNode currentId pos (identifier ++ " $\\gets$ " ++ drawExpr expr1)
    let op = if toInt expr1 <= toInt expr2 then " $<$ " else " $>$ "
    decisionId <- show <$> getNewId
    drawDecisionNode decisionId ("below of=" ++ currentId) (identifier ++ op ++ drawExpr expr2)
    addEdge currentId decisionId "--"

    let crement = if toInt expr1 <= toInt expr2
        then identifier ++ " $\\gets$ " ++ identifier ++ " + 1"
        else identifier ++ " $\\gets$ " ++ identifier ++ " - 1"
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


drawAssignmentTarget :: AssignmentTarget -> String
drawAssignmentTarget (VariableTarget v) = v
drawAssignmentTarget (ListIndexTarget v indexes) =
    v ++ (show $ map (\x -> "[" ++ drawExpr x ++ "]") indexes)
drawAssignmentTarget (StructFieldTarget (StructField x y)) =
    drawExpr x ++ "." ++ drawExpr y

drawAssignmentValue :: AssignmentValue -> String
drawAssignmentValue (ExpressionValue expr) = drawExpr expr
drawAssignmentValue (StructValue (Struct name exprs)) = name ++ "(" ++ intercalateExprs exprs ++ ")"


-- Loop helpers

drawLoopStmts :: [Statement] -> Int -> Flowchart ()
drawLoopStmts stmts coreNodeId = do
    setCoreId coreNodeId
    (currentId, parentId) <- getNextEdge
    case length stmts of
        0 -> return ()
        1 -> do
            drawStmt (head stmts) currentId ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ parentId)
            addEdge parentId currentId "-- node[anchor=east, yshift=0.1cm]{true}"
            addEdge (show $ coreNodeId + 1) (show coreNodeId) "-|" -- med mindre main stmt (den før head stmts) er loop!
        n -> do
            drawStmt (head stmts) currentId ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ parentId)
            addEdge parentId currentId "-- node[anchor=east, yshift=0.1cm]{true}"
            drawStmts (tail stmts)
            addEdge (show $ coreNodeId + n) (show coreNodeId) "-|" -- med mindre main stmt (den før head stmts) er loop!

drawIfStmts :: [Statement] -> Flowchart ()
drawIfStmts [] = return ()
drawIfStmts (loop@(Loop _ _) : stmt : stmts) = do
    drawStmts [loop, stmt]
    currentId <- getLastId
    checkActiveBranch stmt currentId
    drawIfStmts stmts

-- drawIfStmts (cond@(If _ _ _) : stmt : stmts) = do
--     drawStmts [cond, stmt]
--     currentId <- getLastId
--     checkActiveBranch stmt currentId
--     drawIfStmts stmts

drawIfStmts (loop@(ForEach _ _ _) : stmt : stmts) = do
    drawStmts [loop, stmt]
    currentId <- getLastId
    checkActiveBranch stmt currentId
    drawIfStmts stmts

drawIfStmts (loop@(For _ _ _ _) : stmt : stmts) = do
    drawStmts [loop, stmt]
    currentId <- getLastId
    checkActiveBranch stmt currentId
    drawIfStmts stmts

drawIfStmts (stmt : []) = do
    initiateStmt stmt
    currentId <- getLastId
    checkActiveBranch stmt currentId

drawIfStmts (stmt : stmts) = do
    initiateStmt stmt
    drawIfStmts stmts

--     case x of
--         Return expr -> return ()
--         _ -> return ()
            -- legg til i 


-- drawIfStmts stmts = drawStmts stmts

-- drawHeadIfStmt :: [Statement] -> (Maybe Else) -> Flowchart ()
-- drawHeadIfStmt [] Nothing = return ()
-- drawHeadIfStmt (x : xs) Nothing = do
--     (currentId, parentId) <- getNextEdge
--     drawStmt x currentId ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ parentId)
--     addEdge parentId currentId "-- node[anchor=east, yshift=0.1cm]{true}"
--     case xs of
--         [] -> case x of
--             Return _ -> return ()
--             _ -> return ()
                -- add currentId to a record
                -- later, add edge from all ids in this record to the next stmt outside if!
        -- _ -> drawIfStmts xs Nothing

-- må ha en til case med (Just Else) og (Just ElseIf)

-- drawIfStmts :: [Statement] -> (Maybe Else) -> Flowchart ()
-- drawIfStmts (returnExpr@(Return expr) : []) Nothing = do
--     (currentId, parentId) <- getNextEdge
--     drawStmt returnExpr currentId ("below of=" ++ parentId)
--     addEdge parentId currentId "--"

-- drawIfStmts (x : []) Nothing = do
--     (currentId, parentId) <- getNextEdge
--     drawStmt x currentId ("below of=" ++ parentId)
--     addEdge parentId currentId "--"
    -- add currentId to a record
    -- later, add edge from all ids in this record to the next stmt outside if!

-- drawIfStmts (x : xs) Nothing = do
    -- case x of
    --     (If _ _ _) -> do
    --         currentId <- show <$> getNewId
    --         currentCoreId <- show <$> popCoreId
    --         drawStmt x currentId ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ currentCoreId)
    --         addEdge currentCoreId currentId "-- node[anchor=east, yshift=0.1cm]{false}"
    --         drawIfStmts xs Nothing
    --     _ -> do
        -- (currentId, parentId) <- getNextEdge
        -- drawStmt x currentId ("below of=" ++ parentId)
        -- addEdge parentId currentId "--"
        -- drawIfStmts xs Nothing


-- drawIfStmts stmts maybeElse = do
    -- (currentId, parentId) <- getNextEdge
    -- return ()

-- drawElse :: Else -> ..
-- drawElse maybeElse


drawForEachStmts :: [Statement] -> String -> String -> Int -> Flowchart ()
drawForEachStmts stmts identifier collection coreNodeId = do
    setCoreId coreNodeId
    (currentId, parentId) <- getNextEdge
    drawStatementNode currentId ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ parentId) (identifier ++ " $\\gets$ next element in " ++ collection)
    addEdge parentId currentId "-- node[anchor=west, yshift=0.1cm]{false}"
    case length stmts of
        0 -> return ()
        n -> do
            drawStmts stmts
            addEdge (show $ coreNodeId + n + 1) (show coreNodeId) "-|"

drawForStmts :: [Statement] -> Int -> String -> Flowchart ()
drawForStmts stmts coreNodeId crement = do
    setCoreId coreNodeId
    (currentId, parentId) <- getNextEdge
    case length stmts of
        0 -> return ()
        1 -> do
            drawStmt (head stmts) currentId ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ parentId)
            addEdge parentId currentId "-- node[anchor=west, yshift=0.1cm]{true}"

            crementId <- show <$> getNewId
            drawStatementNode crementId ("xshift=3cm, below right of=" ++ currentId) crement
            addEdge currentId crementId "|-"
            addEdge crementId (show coreNodeId) "|-"
        _ -> do
            drawStmt (head stmts) currentId ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ parentId)
            addEdge parentId currentId "-- node[anchor=west, yshift=0.1cm]{true}"
            drawStmts (tail stmts)

            (crementId, crementParentId) <- getNextEdge
            drawStatementNode crementId ("xshift=3cm, below right of=" ++ crementParentId) crement
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
    (Stack edges' _ _ _) <- get
    tell "\n" >> mapM_ tell (reverse edges')

addEdge :: String -> String -> String -> Flowchart ()
addEdge fromId toId direction = do
    let newEdge = "\\draw [edge] (" ++ fromId ++ ") " ++  direction ++ " (" ++ toId ++ ");\n"
    modify (\stack -> stack { edges = newEdge : (edges stack) })

addEdgeFromActiveBranches :: Flowchart ()
addEdgeFromActiveBranches = do
    branches <- getActiveBranches
    currId <- getLastId

    mapM_ (\x -> addEdge x currId "--") branches
    emptyActiveBranches

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
