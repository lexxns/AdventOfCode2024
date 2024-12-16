module Days.Day19 (main) where
import Util (readInputFile)

main :: IO ()
main = do
    input <- readInputFile "day19.txt"
    let grid = lines input
    print "hi"