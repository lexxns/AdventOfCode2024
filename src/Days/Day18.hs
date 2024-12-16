module Days.Day18 (main) where
import Util (readInputFile)

main :: IO ()
main = do
    input <- readInputFile "day18.txt"
    let grid = lines input
    print "hi"