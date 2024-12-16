module Days.Day20 (main) where
import Util (readInputFile)

main :: IO ()
main = do
    input <- readInputFile "day20.txt"
    let grid = lines input
    print "hi"