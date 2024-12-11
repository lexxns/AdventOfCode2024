module TestUtils where

data TestCase a = TestCase {
    testName :: String,
    expected :: a,
    actual :: a
}

runTest :: (Show a, Eq a) => TestCase a -> IO ()
runTest (TestCase name expected actual) = do
    putStr "Test: "
    putStrLn name
    if expected == actual
        then putStrLn "  Passed"
        else do
            putStrLn "  Failed"
            putStr "  Expected: "
            print expected
            putStr "  Got: "
            print actual

runTests :: (Show a, Eq a) => [TestCase a] -> IO ()
runTests tests = do
    putStrLn "Running tests..."
    mapM_ runTest tests