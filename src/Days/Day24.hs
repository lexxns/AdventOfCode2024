module Days.Day24 (main) where
import Util (readInputFile)

main :: IO ()
main = do
    input <- readInputFile "day24.txt"
    let grid = lines input
    print "hi"