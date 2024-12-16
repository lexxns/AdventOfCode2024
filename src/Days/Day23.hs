module Days.Day23 (main) where
import Util (readInputFile)

main :: IO ()
main = do
    input <- readInputFile "day23.txt"
    let grid = lines input
    print "hi"