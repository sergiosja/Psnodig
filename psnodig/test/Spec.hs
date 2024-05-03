import Parsers.Gourmet (testGourmetParser)
import Writers.Gourmet (testGourmetWriter)
import Writers.Pytite (testPytiteWriter)
import Writers.Pseudocode (testPseudocodeWriter)
import Test.HUnit

main :: IO ()
main = do
    putStrLn "\n\n########################################"

    putStrLn "TESTING PSNODIG PARSERS\n"
    mapM_ (\parserTest -> parserTest >>= putStrLn) testSuiteParsers

    putStrLn "\n\n########################################"

    putStrLn "\n\nTESTING PSNODIG WRITERS\n"
    mapM_ (\writerTest -> writerTest >>= putStrLn) testSuiteWriters


runTest :: String -> String -> Test -> IO String
runTest name testPart currentTest = do
    putStrLn $ "\n" ++ name ++ " " ++ testPart ++ ":"
    runTestTT currentTest >>= return . show

testSuiteParsers :: [IO String]
testSuiteParsers =
    [ runTest "Gourmet" "Parser" testGourmetParser ]

testSuiteWriters :: [IO String]
testSuiteWriters =
    [ runTest "Gourmet" "Writer" testGourmetWriter
    , runTest "Pytite" "Writer" testPytiteWriter
    , runTest "Pseudocode" "Writer" testPseudocodeWriter
    ]