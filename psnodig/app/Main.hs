module Main (main) where

-- Psnodig syntax
import Syntax

-- Petite lang
-- import Petite.PetiteParser (parsePetite)
-- import Petite.PetiteTranspiler (transpilePetite)

-- Gourmet lang
import Gourmet.GourmetParser (parseGourmet)
-- import Gourmet.GourmetWriter (writeGourmet)

-- LaTeX
import LaTeX.LatexWriter (writeLatex)
-- import LaTeX.LatexEnv (extractEnv)

-- External imports
import System.Environment (getArgs)
import System.Process (callCommand)
import System.Exit (die)
import Text.Parsec
import Text.Parsec.String
import Control.Monad.Writer

type Transpiler = Program -> Writer String ()

gourmet2tex :: String -> String
gourmet2tex s = take (length s - 2) s ++ "tex"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            p <- readFile file
            transpile p file parseGourmet writeLatex
        _ -> die "Usage:\n stack run -- <fromLang> <toLang> <file>"

transpile :: String -> String -> Parser Program -> Transpiler -> IO ()
transpile program file fromLang toLang = do
    let parsed = parse fromLang "" program
    case parsed of
        (Right p) -> do
            let transpiled = execWriter $ toLang p
            let texfile = gourmet2tex file
            writeFile texfile transpiled
            callCommand $ "pdflatex " ++ texfile
        (Left err) -> putStrLn $ show err
