module LaTeX.Flowcharts
    ( Environment(..)
    , drawFlowchart
    , drawFunctionDecl
    , drawStmts
    , drawExpr
    , drawStartstopNode
    , drawStatementNode
    , drawDecisionNode
    , lastElseIsReturn
    , stmtCount
    , drawOpCrementHelper
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate, sortOn)
import Text.Read (readMaybe)
import Control.Monad.Writer
import Control.Monad.State
import Syntax

data Environment = Environment
    { edges :: [(Int, String)]
    , lastId :: Int
    , coreIds :: [Int]
    , activeBranches :: [String]
    }

type Flowchart = StateT Environment (Writer String)

-- Helper functions

getLastId :: Flowchart String
getLastId = do
    (Environment _ currentId _ _) <- get
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
    currStack@(Environment { coreIds = ids }) <- get
    put currStack { coreIds = (newCoreId : ids) }

popCoreId :: Flowchart Int
popCoreId = do
    currStack@(Environment { coreIds = ids }) <- get
    case ids of
        [] -> error "Flowchart error: Called `popCoreId` with empty coreIds stack"
        (currentCoreId : rest) -> do
            put currStack { coreIds = rest }
            return currentCoreId

peekCoreId :: Flowchart String
peekCoreId = do
    (Environment _ _ core _) <- get
    return . show . head $ core


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

toMaybeInt :: Expression -> Maybe Int
toMaybeInt x = readMaybe (drawExpr x) :: Maybe Int

hasElse :: Maybe Else -> Bool
hasElse Nothing = False
hasElse (Just (Else _)) = True
hasElse (Just (ElseIf _ _ maybeElse)) = hasElse maybeElse

lastElseIsReturn :: Maybe Else -> Bool
lastElseIsReturn Nothing = False
lastElseIsReturn (Just (Else stmts)) =
    case last stmts of { Return _ -> True ; _ -> False }
lastElseIsReturn (Just (ElseIf _ _ maybeElse)) =
    lastElseIsReturn maybeElse

stmtCount :: [Statement] -> Int
stmtCount [] = 0
stmtCount (x:xs) =
    case x of
        Loop _ stmts -> sum [1, stmtCount stmts, stmtCount xs]
        If _ stmts maybeElse -> sum [1, stmtCount stmts, stmtCount xs] +
            case maybeElse of
                Just (ElseIf expr stmts' maybeElse') -> 1 + stmtCount [(If expr stmts' maybeElse')]
                Just (Else stmts') -> 1 + stmtCount stmts'
                Nothing -> 0
        ForEach _ _ stmts -> sum [1, stmtCount stmts, stmtCount xs]
        For _ _ _ stmts -> sum [1, stmtCount stmts, stmtCount xs]
        HashStmt _ -> stmtCount xs
        _ -> 1 + stmtCount xs

addTrailingBranches :: Statement -> Int -> Int -> Flowchart Int
addTrailingBranches stmt currentId numberOfBranches =
    case stmt of
        If _ stmts maybeElse -> do
            let trailingId = currentId + stmtCount stmts

            case (maybeElse, last stmts) of
                (Just (ElseIf expr stmts' maybeElse'), Return _) ->
                    addTrailingBranches (If expr stmts' maybeElse') (trailingId + 1) numberOfBranches

                (Just (ElseIf expr stmts' maybeElse'), _) -> do
                    addBranch (show trailingId)
                    addTrailingBranches (If expr stmts' maybeElse') (trailingId + 1) (numberOfBranches + 1)

                (_, Return _) ->
                    return numberOfBranches

                _ -> do
                    addBranch (show trailingId)
                    return $ 1 + numberOfBranches
        _ -> error "Flowchart error: `addTrailingBranches` called from illegal context"


addBranch :: String -> Flowchart ()
addBranch newId = do
    currStack@(Environment { activeBranches = ids }) <- get
    put currStack { activeBranches = (newId : ids) }

drawBranches :: Int -> String -> Flowchart ()
drawBranches count toId = do
    (Environment _ _ _ ids) <- get
    mapM_ (\x -> addEdge x toId "--") (take count ids)

removeBranches :: Int -> Flowchart ()
removeBranches count = do
    currStack@(Environment { activeBranches = ids }) <- get
    put currStack { activeBranches = drop count ids }


drawOpCrementHelper :: Expression -> Expression -> String -> (String, String)
drawOpCrementHelper expr1 expr2 identifier =
    case (toMaybeInt expr1, toMaybeInt expr2) of
        (Just n, Just m) ->
            let op = if n <= m then " $<$ " else " $>$ "
                crement = identifier ++ " $\\gets$ " ++ identifier ++
                    (if n <= m then " + 1" else " - 1")
            in (op, crement)
        _ -> (" $<$ ", identifier ++ " $\\gets$ " ++ identifier ++ " + 1")


isSpecialStmt :: Statement -> Bool
isSpecialStmt (Loop _ _) = True
isSpecialStmt (If _ _ _) = True
isSpecialStmt (ForEach _ _ _) = True
isSpecialStmt (For _ _ _ _) = True
isSpecialStmt _ = False


-- Values

drawValue :: Value -> String
drawValue Nil = "Nil"
drawValue (Boolean b) = show b
drawValue (Number n) = show n
drawValue (Decimal d) = show d
drawValue (Text t) = show t
drawValue (List l) = drawIterable "[" l "]"
drawValue (HashSet hs) = drawIterable "(" (Set.toList hs) ")"
drawValue (HashMap hm) = drawHMap $ Map.toList hm
drawValue (StructVal sf) = drawStruct sf

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
            let val' = drawValue val
            in str ++ ": " ++ val'


-- Expressions

drawExpr :: Expression -> String
drawExpr (Constant v) = drawValue v
drawExpr (VariableExp v) = v
drawExpr (BinaryExp op expr1 expr2) = drawExpr expr1 ++ drawOp op ++ drawExpr expr2
drawExpr (ListIndex listName exprs) = listName ++ drawIndexExprs exprs
drawExpr (CallExp functionCall) = drawFunctionCall functionCall
drawExpr (Not expr) = "not " ++ drawExpr expr
drawExpr (StructExpr (Struct structName exprs)) = structName ++ "(" ++ intercalateExprs exprs ++ ")"
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
drawOp NotEqual = " $\\neq$ "

drawOp And = " $\\land$ "
drawOp Or = " $\\lor$ "
drawOp Modulo = " $\\%$ "

drawIndexExprs :: [Expression] -> String
drawIndexExprs [] = ""
drawIndexExprs (x:xs) = "[" ++ drawExpr x ++ "]" ++ drawIndexExprs xs


-- Statements

initiateStmtStraight :: Statement -> Flowchart ()
initiateStmtStraight stmt = do
    (currentId, parentId) <- getNextEdge
    drawStmt stmt currentId ("below of=" ++ parentId)
    addEdge parentId currentId "--"

initiateStmtRight :: Statement -> Flowchart ()
initiateStmtRight stmt = do
    currentId <- show <$> getNewId
    currentCoreId <- show <$> popCoreId
    drawStmt stmt currentId ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ currentCoreId)
    addEdge currentCoreId currentId "-- node[anchor=west, yshift=0.1cm]{false}"

drawStmts :: [Statement] -> Bool -> Flowchart ()
drawStmts [] _ = return ()

drawStmts (stmt@(If _ _ maybeElse) : x : xs) fromIf = do
    startId <- (+ 1) . (read :: String -> Int) <$> getLastId

    if fromIf then initiateStmtRight stmt
    else initiateStmtStraight stmt

    -- Puts the next statement underneath the first ending if-else without a return
    if hasElse maybeElse && lastElseIsReturn maybeElse
    then do
        drawSpecialIf stmt x startId
        drawStmts xs False
    else
        drawStmts (x:xs) (not $ hasElse maybeElse)

    -- in the case where else has a return, and is the last stmt in the list,
    --     don't draw edges to the `next` stmt, because there isn't one!

drawStmts (stmt@(Loop _ _) : rest) fromIf = do
    if fromIf then initiateStmtRight stmt
    else initiateStmtStraight stmt
    drawStmts rest True

drawStmts (stmt@(ForEach _ _ _) : x : xs) fromIf = do
    if fromIf then initiateStmtRight stmt
    else initiateStmtStraight stmt

    -- Left side of loop
    currentId <- show <$> getNewId
    currentCoreId <- show <$> popCoreId
    drawStmt x currentId ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ currentCoreId)
    addEdge currentCoreId currentId "-- node[anchor=east, yshift=0.1cm]{true}"
    drawStmts xs False

    -- hvis første x' i xs er ForEach, eller noe annet, så bør den også bli tegnet
    -- som en `left side of loop`, istedenfor vanlig `initiateStmtStraight`!!

    -- ok, for loops går det. cluet er å ha alt på samme side. da kan man bruke initiateStmtRight som `initiateSpecialStmt` ellerno
    -- kan også endre `fromIf` til `fromSpecial`. bør egt finne et kult navn for disse 4
    -- feks `fromDecision`, `fromSpecial`, `fromCompund` ellerno! :))

drawStmts (stmt@(For _ _ _ _) : x : xs) fromIf = do
    if fromIf then initiateStmtRight stmt
    else initiateStmtStraight stmt

    -- Left side of loop
    currentId <- show <$> getNewId
    currentCoreId <- show <$> popCoreId
    drawStmt x currentId ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ currentCoreId)
    addEdge currentCoreId currentId "-- node[anchor=east, yshift=0.1cm]{false}"
    drawStmts xs False

    -- hvis første x' i xs er For, eller noe annet, så bør den også bli tegnet
    -- som en `left side of loop`, istedenfor vanlig `initiateStmtStraight`!!

drawStmts (stmt : stmts) fromIf =
    case stmt of
        HashStmt _ -> drawStmts stmts False
        AnnotationStmt "" _ -> drawStmts stmts False
        _ -> do
            if fromIf then initiateStmtRight stmt
            else initiateStmtStraight stmt
            drawStmts stmts False

drawStmt :: Statement -> String -> String -> Flowchart ()
drawStmt (Assignment target expr) currentId pos =
    drawStatementNode currentId pos (drawAssignmentVar target ++ " $\\gets$ " ++ drawExpr expr)

drawStmt (Loop expr stmts) currentId pos = do
    drawDecisionNode currentId pos (drawExpr expr)
    drawLoopStmts stmts (read currentId :: Int)

drawStmt stmt@(If expr stmts maybeElse) currentId pos = do
    trailingBranches <- addTrailingBranches stmt (read currentId :: Int) 0

    drawDecisionNode currentId pos (drawExpr expr)
    setCoreId (read currentId :: Int)
    drawIfStmts stmts currentId maybeElse

    -- Adds edges from all ending if-else without return to the next statement
    endId <- (+ 1) . (read :: String -> Int) <$> getLastId
    drawBranches trailingBranches (show endId)
    removeBranches trailingBranches

drawStmt (ForEach identifier expr stmts) currentId pos = do
    let collection = drawExpr expr
    drawDecisionNode currentId pos ("Iterated " ++ collection)
    drawForEachStmts stmts identifier collection (read currentId :: Int)

drawStmt (For identifier expr1 expr2 stmts) currentId pos = do
    drawStatementNode currentId pos (identifier ++ " $\\gets$ " ++ drawExpr expr1)
    let (op, crement) = drawOpCrementHelper expr1 expr2 identifier
    decisionId <- show <$> getNewId

    -- This is the node under the one with identifier <- expr1
    drawDecisionNode decisionId ("below of=" ++ currentId) (identifier ++ op ++ drawExpr expr2)
    addEdge currentId decisionId "--"

    drawForStmts stmts (read decisionId :: Int) crement

drawStmt (CallStmt functionCall) currentId pos =
    drawStatementNode currentId pos (drawFunctionCall functionCall)

drawStmt (Return expr) currentId pos = do
    drawStartstopNode currentId pos (drawExpr expr)

drawStmt (HashStmt _) _ _ = return ()
drawStmt (AnnotationStmt text _) currentId pos =
    drawStatementNode currentId pos text

drawStmt Break currentId pos =
    drawStatementNode currentId pos "break"
drawStmt Continue currentId pos =
    drawStatementNode currentId pos "continue"

drawAssignmentVar :: AssignmentVar -> String
drawAssignmentVar (VariableTarget v) = v
drawAssignmentVar (ListIndexTarget v indexes) = v ++ drawIndexExprs indexes
drawAssignmentVar (StructFieldTarget (StructField x y)) =
    drawExpr x ++ "." ++ drawExpr y


-- Special statement helpers

drawLoopStmts :: [Statement] -> Int -> Flowchart ()
drawLoopStmts stmts coreNodeId = do
    setCoreId coreNodeId
    (currentId, parentId) <- getNextEdge
    case length stmts of
        0 -> return ()
        1 -> do
            drawStmt (head stmts) currentId ("yshift=-0.5cm, xshift=-3cm, below left of=" ++ parentId)
            addEdge parentId currentId "-- node[anchor=east, yshift=0.1cm]{true}"
            lastStmt <- getLastId
            addEdge lastStmt (show coreNodeId) "-|"
        _ -> do
            drawStmt (head stmts) currentId ("yshift=-1cm, xshift=-4cm, below left of=" ++ parentId)
            addEdge parentId currentId "-- node[anchor=east, yshift=0.1cm]{true}"
            drawStmts (tail stmts) (isSpecialStmt (head stmts))
            lastStmt <- getLastId
            addEdge lastStmt (show coreNodeId) "-|"

drawIfStmts :: [Statement] -> String -> Maybe Else -> Flowchart ()
drawIfStmts stmts currentId maybeElse =
    case stmts of
        [] -> return ()
        (x : xs) -> do
            headId <- show <$> getNewId
            drawStmt x headId ("yshift=-0.5cm, xshift=-1.5cm, below left of=" ++ currentId)
            addEdge currentId headId "-- node[anchor=east, yshift=0.1cm]{true}"

            drawStmts xs False
    
            case maybeElse of
                Just (ElseIf expr' stmts' maybeElse') ->
                    drawStmts [(If expr' stmts' maybeElse')] True
                Just (Else stmts') ->
                    drawStmts stmts' True
                Nothing -> return ()

drawSpecialIf :: Statement -> Statement -> Int -> Flowchart ()
drawSpecialIf (If expr stmts maybeElse) toBePrinted accumulatedId = do
    let parentId = stmtCount stmts + accumulatedId
    case last stmts of
        Return _ ->
            case maybeElse of
                Nothing -> error "Flowchart error: Unreachable statement cannot be drawn properly."
                Just (Else stmts') ->
                    drawSpecialIf (If expr stmts' Nothing) toBePrinted (parentId + 1)
                Just (ElseIf _ stmts' maybeElse') ->
                    drawSpecialIf (If expr stmts' maybeElse') toBePrinted (parentId + 1)
        _ -> do
            currentId <- show <$> getNewId
            drawStmt toBePrinted currentId ("below of=" ++ (show parentId))
drawSpecialIf _ _ _ = error "Flowchart error: `drawSpecialIf` called from illegal context."


drawForEachStmts :: [Statement] -> String -> String -> Int -> Flowchart ()
drawForEachStmts stmts identifier collection coreNodeId = do
    setCoreId coreNodeId
    (currentId, parentId) <- getNextEdge
    drawStatementNode currentId ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ parentId) (identifier ++ " $\\gets$ next element in " ++ collection)
    addEdge parentId currentId "-- node[anchor=west, yshift=0.1cm]{false}"
    case length stmts of
        0 -> return ()
        n -> do
            drawStmts stmts False -- kjør isSpecialStmt her?
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
            drawForCrement crementId currentId crement
        _ -> do
            drawStmt (head stmts) currentId ("yshift=-0.5cm, xshift=1.5cm, below right of=" ++ parentId)
            addEdge parentId currentId "-- node[anchor=west, yshift=0.1cm]{true}"
            drawStmts (tail stmts) False
            (crementId, crementParentId) <- getNextEdge
            drawForCrement crementId crementParentId crement

drawForCrement :: String -> String -> String -> Flowchart ()
drawForCrement crementId parentId crement = do
    coreNodeId <- peekCoreId
    drawStatementNode crementId ("xshift=3cm, below right of=" ++ parentId) crement
    addEdge parentId crementId "|-"
    addEdge crementId coreNodeId "|-"


-- Functions

drawFunctionDecl :: FunctionDecl -> Flowchart ()
drawFunctionDecl (FunctionDecl name args stmts) = do
    tell $ "\\node (0) [startstop] {" ++ name ++ "(" ++ intercalateArgs args ++ ")};\n"
    if null stmts then return ()
    else drawStmts stmts False

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
    (Environment edges' _ _ _) <- get
    tell "\n" >> mapM_ (\(_, e) -> tell e) (sortOn fst edges')

addEdge :: String -> String -> String -> Flowchart ()
addEdge fromId toId direction = do
    let newEdge = "\\draw [edge] (" ++ fromId ++ ") " ++ direction ++ " (" ++ toId ++ ");\n"
    modify (\stack -> stack { edges = (read fromId :: Int, newEdge) : (edges stack) })


-- Entry point

constantConfig :: Flowchart ()
constantConfig = do
    tell "\\documentclass[margin=3mm]{standalone}\n\\usepackage{tikz}\n\\usetikzlibrary{shapes, arrows}\n\n"
    tell "\\tikzstyle{startstop} = [rectangle, rounded corners, minimum width=1cm, minimum height=1cm, text centered, draw=black, text=white, fill=black!80]\n"
    tell "\\tikzstyle{statement} = [rectangle, minimum width=1cm, minimum height=1cm, text centered, draw=black, fill=blue!20]\n"
    tell "\\tikzstyle{decision} = [rectangle, minimum height=1cm, text centered, draw=black, fill=yellow!30]\n"
    tell "\\tikzstyle{edge} = [thick, ->, >=stealth]\n\n"
    tell "\\begin{document}\n\\begin{tikzpicture}[node distance=2cm]\n\n"

drawFlowchart :: Program -> Flowchart ()
drawFlowchart (Program _ _ funcs _) =
    if null funcs then return ()
    else do
        constantConfig
        drawFunctionDecl $ head funcs
        drawEdges
        tell "\n\\end{tikzpicture}\n\\end{document}"
