-- src/Util.hs
module Util
    ( readInputFile
    , readInputFileLines
    , getInputPath
    , dropLast
    , numDigits
    ) where

import System.FilePath ((</>), takeDirectory) 
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory, doesFileExist)
import Control.Exception (catch, SomeException)
import System.IO.Unsafe (unsafePerformIO)

getResourcesPath :: IO FilePath
getResourcesPath = do
    currentDir <- getCurrentDirectory
    findProjectRoot currentDir
  where
    findProjectRoot dir = do
        let packageYaml = dir </> "package.yaml"
        hasPackageYaml <- doesFileExist packageYaml
        if hasPackageYaml
            then return $ dir </> "src\\Resources"
            else do
                let parentDir = takeDirectory dir
                if parentDir == dir
                    then error "Could not find project root (no package.yaml found)"
                    else findProjectRoot parentDir

getInputPath :: String -> IO FilePath
getInputPath filename = do
    resourcesPath <- getResourcesPath
    return $ resourcesPath </> filename

readInputFile :: String -> IO String
readInputFile filename = do
    path <- getInputPath filename
    catch (readFile path) handleError
  where
    handleError :: SomeException -> IO String
    handleError e = do
        putStrLn $ "Error reading input file '" ++ filename ++ "': " ++ show e
        return ""

-- Read the input file as a list of lines
readInputFileLines :: String -> [String]
readInputFileLines filename = lines $ unsafePerformIO $ readInputFile filename

dropLast :: [a] -> [a]
dropLast    []  =  []
dropLast    xs  =  init xs

numDigits :: Int -> Int
numDigits n | n < 0 = numDigits (-n)
            | n < 10 = 1
            | otherwise = 1 + numDigits (n `div` 10)