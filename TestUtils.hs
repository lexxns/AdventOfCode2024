module TestUtils where

data TestCase a = TestCase {
    testName :: String,
    expected :: a,
    actual :: a
}

runTest :: (Show a, Eq a) => TestCase a -> IO ()
runTest (TestCase name expected actual) = do
    putStrLn $ "Test: " ++ name
    if expected == actual
        then putStrLn "  ✓ Passed"
        else putStrLn $ "  ✗ Failed (Expected: " ++ show expected ++ ", Got: " ++ show actual ++ ")"
    putStrLn ""

runTests :: (Show a, Eq a) => [TestCase a] -> IO ()
runTests tests = do
    putStrLn "Running tests...\n"
    mapM_ runTest tests
    putStrLn "All tests completed!"