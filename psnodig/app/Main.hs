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

-- External imports
import System.Environment (getArgs)
import System.Exit (die)
import Text.Parsec
import Text.Parsec.String
import Control.Monad.Writer
import Data.Maybe (isJust, fromJust)

type Transpiler = Program -> Writer String ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            p <- readFile file
            transpile p parseGourmet writeLatex
        -- ["a", fromLang, toLang, file] -> do
        --     p <- readFile file
        --     let selectedParser = selectParser fromLang
        --         selectedTranspiler = selectTranspiler toLang        
        --     case (isJust selectedParser) && (isJust selectedTranspiler) of
        --         True -> transpile p (fromJust selectedParser) (fromJust selectedTranspiler)
        --         False -> die $ "Either " ++ fromLang ++ " or " ++ toLang ++ " not found."
        -- ["b", parser, file] -> do
        --     p <- readFile file
        --     let selectedParser = selectParser parser
        --     case (isJust selectedParser) of
        --         True -> buildAST p (fromJust selectedParser)
        --         False -> die $ "Parser " ++ parser ++ " not found."
        _ -> die "Usage:\n stack run -- <fromLang> <toLang> <file>"


-- selectParser :: String -> Maybe (Parser Program)
-- selectParser "Gourmet" = Just parseGourmet
-- selectParser "Petite" = Just parsePetite
-- selectParser _ = Nothing

-- selectTranspiler :: String -> Maybe Transpiler
-- selectTranspiler "Gourmet" = Just writeGourmet
-- selectTranspiler "Petite" = Just transpilePetite
-- selectTranspiler "LaTeX" = Just writeLatex
-- selectTranspiler _ = Nothing

transpile :: String -> Parser Program -> Transpiler -> IO ()
transpile program fromLang toLang = do
    let parsed = parse fromLang "" program
    case parsed of
        (Right p) -> do
            let transpiled = execWriter $ toLang p
            writeFile "output.txt" transpiled
        (Left err) -> putStrLn $ show err

-- buildAST :: String -> Parser Program -> IO ()
-- buildAST program parser = do
--     let parsed = parse parser "" program
--     case parsed of
--         (Right p) -> print p
--         (Left err) -> putStrLn $ show err