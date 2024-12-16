module Days.Day22 (main) where
import Util (readInputFile)

main :: IO ()
main = do
    input <- readInputFile "day22.txt"
    let grid = lines input
    print "hi"