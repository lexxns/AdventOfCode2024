module Util where

dropLast :: [a] -> [a]
dropLast    []  =  []
dropLast    xs  =  init xs

numDigits :: Int -> Int
numDigits n | n < 0 = numDigits (-n)
            | n < 10 = 1
            | otherwise = 1 + numDigits (n `div` 10)