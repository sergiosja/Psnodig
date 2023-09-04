module Main (main) where

-- Psnodig syntax
import Syntax

-- Petite lang
import Petite.PetiteParser (parsePetite)
import Petite.PetiteTranspiler (transpilePetite)

-- Gourmet lang
import Gourmet.GourmetParser (parseGourmet)
import Gourmet.GourmetTranspiler (transpileGourmet)

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
        [fromLang, toLang, file] -> do
            p <- readFile file
            let selectedParser = selectParser fromLang
                selectedTranspiler = selectTranspiler toLang        
            if (isJust selectedParser) && (isJust selectedTranspiler)
            then transpile p (fromJust selectedParser) (fromJust selectedTranspiler)
            else die $ "Either " ++ fromLang ++ " or " ++ toLang ++ " not found."
        [fromLang, file] -> do
            p <- readFile file
            let selectedParser = selectParser fromLang
            if (isJust selectedParser)
            then getAST p (fromJust selectedParser)
            else die $ fromLang ++ " parser not found."
        _ -> die "Usage:\n stack run -- <fromLang> <toLang> <file>"

selectParser :: String -> Maybe (Parser Program)
selectParser "Gourmet" = Just parseGourmet
selectParser "Petite" = Just parsePetite
selectParser _ = Nothing

selectTranspiler :: String -> Maybe Transpiler
selectTranspiler "Gourmet" = Just transpileGourmet
selectTranspiler "Petite" = Just transpilePetite
selectTranspiler _ = Nothing

transpile :: String -> Parser Program -> Transpiler -> IO ()
transpile program fromLang toLang = do
    let parsed = parse fromLang "" program
    case parsed of
        (Left err) -> putStrLn $ show err
        (Right p) -> do
            let transpiled = execWriter $ toLang p
            writeFile "output.txt" transpiled

getAST :: String -> Parser Program -> IO ()
getAST program fromLang = do
    let parsed = parse fromLang "" program
    case parsed of
        (Left err) -> putStrLn $ show err
        (Right p) -> print p