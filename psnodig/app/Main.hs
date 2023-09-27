module Main (main) where

-- Psnodig syntax
import Syntax

-- Petite lang
-- import Petite.PetiteParser (parsePetite)
-- import Petite.PetiteTranspiler (transpilePetite)

-- Gourmet lang
import Gourmet.GourmetParser (parseGourmet)
import Gourmet.GourmetWriter (writeGourmet)

-- LaTeX
import LaTeX.LatexWriter (writeLatex)
import LaTeX.LatexEnv (extractEnv)

-- External imports
import System.Environment (getArgs)
import System.Process (callCommand)
import System.Exit (die)
import Text.Parsec
-- import Text.Parsec.String
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            p <- readFile filename
            transpile p filename
        ["p", file] -> do
            p <- readFile file
            getAST p
        ["g2g", file] -> do
            p <- readFile file
            g2g p
        _ -> die "Usage:\n stack run -- <file>"

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

getAST :: String -> IO ()
getAST program = do
    let parsed = parse parseGourmet "" program
    case parsed of
        (Right p) -> print p
        (Left err) -> putStrLn $ show err