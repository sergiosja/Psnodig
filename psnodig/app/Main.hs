module Main (main) where

-- Psnodig syntax
import Syntax
import Interpreter (ExecutionState(..), RuntimeError(RuntimeErrorWithOutput), runPsnodig)

-- Pytite lang
-- import Petite.PetiteParser (parsePetite)
-- import Petite.PetiteTranspiler (transpilePetite)

-- Gourmet lang
import Gourmet.GourmetParser (parseGourmet)
import Gourmet.GourmetWriter (writeGourmet)

-- LaTeX
import LaTeX.Flowcharts (Stack(..), writeFlowchart)
import LaTeX.LatexWriter (writeLatex)
import LaTeX.LatexEnv (extractEnv)

-- External imports
-- import qualified Data.Map as Map
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
        [filename] -> do
            p <- readFile filename
            interpret p
        ["tbp", filename] -> do -- should run 'pdf' if user wants pdf too
            p <- readFile filename
            transpile p filename
        ["ibp", filename] -> do
            p <- readFile filename
            makeFlowchart p filename
        ["ast", filename] -> do -- should be able to get this in its own file maybe?
            p <- readFile filename
            getAST p
        ["g2g", filename] -> do
            p <- readFile filename
            g2g p
        _ -> die "Usage:\n stack run -- <filename>" -- gjøre denne stor og fin! feks ved å skrive <stack run -- "help"> ellerno

transpile :: String -> String -> IO ()
transpile program filename = do
    let parsed = parse parseGourmet "" program
    case parsed of
        (Right p) -> do
            let env = extractEnv p
            let transpiled = execWriter $ runReaderT (writeLatex p) env
            let texfile = gourmet2tex filename
            writeFile texfile transpiled
            callCommand $ "latexmk -pdf " ++ texfile
        (Left err) -> putStrLn $ show err

g2g :: String -> IO ()
g2g program = do
    let parsed = parse parseGourmet "" program
    case parsed of
        (Right p) ->
            let transpiled = execWriter $ writeGourmet p
            in writeFile "algo.gt" transpiled
        (Left err) -> putStrLn $ show err

makeFlowchart :: String -> String -> IO ()
makeFlowchart program filename = do
    let parsed = parse parseGourmet "" program
    case parsed of
        Right p -> do
            let flowTex = execWriter $ runStateT (writeFlowchart p) (Stack [] 0 [0] [])
            let texfile = gourmet2flowTex filename
            writeFile texfile flowTex
            callCommand $ "latexmk -pdf " ++ texfile
        Left err -> putStrLn $ show err

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
gourmet2tex s = take (length s - 2) s ++ "tex"

gourmet2flowTex :: String -> String
gourmet2flowTex s = take (length s - 3) s ++ "_flowchart.tex"


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