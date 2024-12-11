import Data.List (foldl', findIndex, nub)
import Data.Either (lefts, isLeft, isRight)


-- Translates input to a list of Left (value) and Right (empty)
-- "12345"
-- [
--   Left 0,                     -- from first number (1)
--   Right 0, Right 0,          -- from second number (2)
--   Left 1, Left 1, Left 1,    -- from third number (3)
--   Right 0, Right 0, Right 0, Right 0,  -- from fourth number (4)
--   Left 2, Left 2, Left 2, Left 2, Left 2  -- from fifth number (5)
-- ]
generateSequence :: String -> [[Either Int Int]]
generateSequence input = filter (not . null) lst
  where
    lst = zipWith processChar [0..] input
    processChar i c
      | even i = replicate (read [c]) (Left (i `div` 2))
      | otherwise = replicate (read [c]) (Right 0)

haslefts :: [Either a b] -> Bool
haslefts = any isLeft

getLeftLists :: [[Either a b]] -> [[Either a b]]
getLeftLists = reverse . filter haslefts

findGap :: Int -> [Either a b] -> Maybe Int
findGap len seq = findIndex (\i -> countConsecutiveRights i seq >= len) [0..length seq]
  where
    countConsecutiveRights start xs = 
      length $ takeWhile isRight $ drop start xs

insertLefts :: [[Either a b]] -> [Either a b] -> [[Either a b]]
insertLefts lefts sequen = 
    map (\leftList -> case findGap (length leftList) sequen of
            Just pos -> take pos sequen ++ leftList ++ drop (pos + length leftList) sequen
            Nothing -> sequen) lefts
