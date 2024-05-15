import Parsers.Gourmet (testGourmetParser)
import Writers.Gourmet (testGourmetWriter)
import Writers.Python (testPythonWriter)
import Writers.Pseudocode (testPseudocodeWriter)
import Writers.Flowchart (testFlowchartWriter)
import InterpreterTest (testInterpreter)

import Test.HUnit
import Syntax


-- Unit testing

testSuiteParsers :: [IO String]
testSuiteParsers =
    [ runTest "Gourmet" "Parser" testGourmetParser ]

testSuiteWriters :: [IO String]
testSuiteWriters =
    [ runTest "Gourmet" "Writer" testGourmetWriter
    , runTest "Python" "Writer" testPythonWriter
    , runTest "Pseudocode" "Writer" testPseudocodeWriter
    , runTest "Flowchart" "Writer" testFlowchartWriter
    ]

runTest :: String -> String -> Test -> IO String
runTest name testPart currentTest = do
    putStrLn $ "\n" ++ name ++ " " ++ testPart ++ ":"
    runTestTT currentTest >>= return . show


-- Entry point

main :: IO ()
main = do
    putStrLn "TESTING INTERPRETER\n"
    runTestTT testInterpreter >>= print

    putStrLn "\n\n########################################\n"

    putStrLn "\nTESTING PSNODIG PARSERS\n"
    mapM_ (\parserTest -> parserTest >>= putStrLn) testSuiteParsers

    putStrLn "\n\n########################################\n"

    putStrLn "\nTESTING PSNODIG WRITERS\n"
    mapM_ (\writerTest -> writerTest >>= putStrLn) testSuiteWriters

    putStrLn "\n\n########################################\n"
