module Main (main) where

-- Psnodig syntax
import Syntax
import Interpreter (ExecutionState(..), runPsnodig)

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
            callCommand "/Users/sergey/Documents/Master/psnodig/src/Programs/cleanup.sh"
        ["ibp", filename] -> do
            p <- readFile filename
            p2f p
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
            callCommand $ "pdflatex " ++ texfile
        (Left err) -> putStrLn $ show err
    where
        gourmet2tex :: String -> String
        gourmet2tex s = take (length s - 2) s ++ "tex"


g2g :: String -> IO ()
g2g program = do
    let parsed = parse parseGourmet "" program
    case parsed of
        (Right p) ->
            let transpiled = execWriter $ writeGourmet p
            in writeFile "algo.gt" transpiled
        (Left err) -> putStrLn $ show err

p2f :: String -> IO ()
p2f program = do
    let parsed = parse parseGourmet "" program
    case parsed of
        Right p -> do
            let flowTex = execWriter $ runStateT (writeFlowchart p) (Stack [("start", [])] 0)
            writeFile "flowchart.tex" flowTex
            callCommand $ "pdflatex flowchart.tex"
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
                (Right finalState) -> forM_ (output finalState) putStrLn 
                (Left err) -> print err
        (Left err) -> putStrLn $ show err



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