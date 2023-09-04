module Main (main) where

-- Petite lang
import Petite.PetiteParser (parsePetite)
import Petite.PetiteTranspiler (transpilePetite)

-- Gourmet lang
import Gourmet.GourmetParser (parseGourmet)
import Gourmet.GourmetTranspiler (transpileGourmet)

-- External imports
import Control.Monad.Writer
import System.Environment (getArgs)
import System.Exit (die)
import Text.Parsec

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-g2p", file] -> do
            p <- readFile file
            gourmet2petite p
        ["-p2g", file] -> do
            p <- readFile file
            petite2gourmet p
        _ ->
            die "Usage:\n\
            \ stack run -- <flag> <file>"

gourmet2petite :: String -> IO ()
gourmet2petite program =
    let parsed = parse parseGourmet "" program
    in case parsed of
        (Left err) -> putStrLn $ show err
        (Right p) -> putStrLn $ execWriter (transpilePetite p)

petite2gourmet :: String -> IO ()
petite2gourmet program =
    let parsed = parse parsePetite "" program
    in case parsed of
        (Left err) -> putStrLn $ show err
        (Right p) -> putStrLn $ execWriter (transpileGourmet p)