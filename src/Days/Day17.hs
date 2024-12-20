module Days.Day17 (main) where
import Util (readInputFile)
    

main :: IO ()
main = do
    input <- readInputFile "day17.txt"
    let grid = lines input
    print "hi"