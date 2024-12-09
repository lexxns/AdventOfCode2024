import Data.List (foldl')
import Data.Either (lefts)


-- Translates input to a list of Left (value) and Right (empty)
-- "12345"
-- [
--   Left 0,                     -- from first number (1)
--   Right 0, Right 0,          -- from second number (2)
--   Left 1, Left 1, Left 1,    -- from third number (3)
--   Right 0, Right 0, Right 0, Right 0,  -- from fourth number (4)
--   Left 2, Left 2, Left 2, Left 2, Left 2  -- from fifth number (5)
-- ]
generateSequence :: String -> [Either Int Int]
generateSequence input = concat $ zipWith processChar [0..] input
  where
    processChar i c
      | even i = replicate (read [c]) (Left (i `div` 2))
      | otherwise = replicate (read [c]) (Right 0)

calculateSum :: [Either Int Int] -> Int
calculateSum input =
    let values = lefts input
        ln = length values

        processPosition :: (Int, [Int]) -> (Int, Either Int Int) -> (Int, [Int])
        processPosition (sum, remaining) (pos, value)
            | pos >= ln = (sum, remaining)
            | otherwise = case value of
                Right _ -> case remaining of 
                    (x:xs) -> (sum + pos * x, xs)
                    [] -> (sum, [])
                Left v -> (sum + pos * v, remaining)

        initialState = (0, reverse values)

        (finalSum, _) = foldl' processPosition initialState (zip [0..] input)
    in finalSum

solve :: String -> Int
solve input = calculateSum $ generateSequence input

main :: IO ()
main = do
    input <- getLine
    print $ solve input
