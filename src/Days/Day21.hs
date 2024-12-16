module Days.Day21 (main) where
import Util (readInputFile)

main :: IO ()
main = do
    input <- readInputFile "day21.txt"
    let grid = lines input
    print "hi"