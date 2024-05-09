module Main (main) where

-- Psnodig syntax
import Syntax
import Interpreter
    ( ExecutionState(..)
    , RuntimeError(RuntimeErrorWithOutput)
    , runPsnodig
    )

-- Pytite
import Pytite.PytiteWriter (writePytite)

-- Gourmet
import Gourmet.GourmetParser (parseGourmet)
import Gourmet.GourmetWriter (writeGourmet)

-- Pseudocode
import LaTeX.LatexWriter (writeLatex)
import LaTeX.LatexEnv (extractEnv)

-- Flowcharts
import LaTeX.Flowcharts
    ( Environment(..)
    , writeFlowchart
    )

-- External imports
import System.Environment (getArgs)
import System.Process (callCommand)
import System.Exit (die)
import Text.Parsec
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer
import Control.Monad.State


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["help"] -> putStrLn psnodigHelp
        [filename] -> do
            p <- readFile filename
            interpret p
        ["tbp", filename] -> do
            p <- readFile filename
            makeTBP p filename False
        ["tbp", "pdf", filename] -> do
            p <- readFile filename
            makeTBP p filename True
        ["ibp", filename] -> do
            p <- readFile filename
            makeIBP p filename False
        ["ibp", "pdf", filename] -> do
            p <- readFile filename
            makeIBP p filename True
        ["ast", filename] -> do -- should be able to get this in its own file maybe?
            p <- readFile filename
            getAST p
        ["gourmet", filename] -> do
            p <- readFile filename
            g2g p
        ["pytite", filename] -> do
            p <- readFile filename
            g2p p
        _ -> die "No parse for provided arguments. Run `psnodig help` to see usage."

psnodigHelp :: String
psnodigHelp =
    "Psnodig - a general transpiler with options for pseudocode and flowcharts.\n\n" ++
    "Available options:\n\n" ++
    "<psnodig program>\t\t: Parses the program and runs it through the interpreter.\n\n" ++
    "<psnodig ast program>\t\t: Prints the program's AST to the terminal.\n\n" ++
    "<psnodig tbp program>\t\t: Transpiles the program to TBP in LaTeX.\n" ++
    "<psnodig tbp pdf program>\t: The same as above, but also compiles the LaTeX file to a PDF.\n\n" ++
    "<psnodig ibp program>\t\t: Transpiles the program to IBP in LaTeX.\n" ++
    "<psnodig ibp pdf program>\t: The same as above, but also compiles the LaTeX file to a PDF.\n\n" ++
    "<psnodig gourmet program>\t: Transpiles the program to the equivalent in Gourmet.\n" ++
    "<psnodig pytite program>\t: Transpiles the program to the equivalent in Pytite.\n\n" ++
    "<psnodig help>\t\t\t: Brings you back here!\n"

makeTBP :: String -> String -> Bool -> IO ()
makeTBP program filename pdf = do
    let parsed = parse parseGourmet "" program
    case parsed of
        (Right p) -> do
            let env = extractEnv p
            let transpiled = execWriter $ runReaderT (writeLatex p) env
            let texfile = gourmet2tex filename
            writeFile texfile transpiled
            if pdf then makePDF texfile else return ()
        (Left err) -> putStrLn $ show err

makeIBP :: String -> String -> Bool -> IO ()
makeIBP program filename pdf = do
    let parsed = parse parseGourmet "" program
    case parsed of
        Right p -> do
            let flowTex = execWriter $ runStateT (writeFlowchart p) (Environment [] 0 [0] [])
            let texfile = gourmet2flowTex filename
            writeFile texfile flowTex
            if pdf then makePDF texfile else return ()
        Left err -> putStrLn $ show err

g2g :: String -> IO ()
g2g program = do
    let parsed = parse parseGourmet "" program
    case parsed of
        (Right p) ->
            let transpiled = execWriter $ writeGourmet p
            in writeFile "algo.gt" transpiled
        (Left err) -> putStrLn $ show err

g2p :: String -> IO ()
g2p program = do
    let parsed = parse parseGourmet "" program
    case parsed of
        (Right p) ->
            let transpiled = execWriter $ writePytite p
            in writeFile "algo.py" transpiled
        (Left err) -> putStrLn $ show err

getAST :: String -> IO ()
getAST program = do
    let parsed = parse parseGourmet "" program
    case parsed of
        (Right p) -> print p
        (Left err) -> putStrLn $ show err

interpret :: String -> IO ()
interpret program = do
    let parsed = parse parseGourmet "" program
    case parsed of
        (Right p) -> do
            res <- runPsnodig p
            case res of
                (Right finalState) ->
                    mapM_ putStrLn (output finalState)
                (Left (RuntimeErrorWithOutput output' err)) -> do
                    mapM_ putStrLn output'
                    print err
                (Left err) ->
                    print err
        (Left err) -> putStrLn $ show err

gourmet2tex :: String -> String
gourmet2tex s = take (length s - 3) s ++ "_tbp.tex"

gourmet2flowTex :: String -> String
gourmet2flowTex s = take (length s - 3) s ++ "_ibp.tex"

makePDF :: String -> IO ()
makePDF filename = do
    callCommand $ "latexmk -pdf " ++ filename

-- kjøre helt IN1000 style med
{-
> Choose parser
1. Gourmet
2. Pytite
3. Other
> 1

> Choose writer
1. Gourmet
2. Pytite
3. Latex
4. Flowchart
> 4

#################
Your new file algo.pdf is ready
-}