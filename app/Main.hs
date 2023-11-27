module Main where

import Control.Monad (unless)
import qualified Latte.Analysis.StaticAnalysis as SA
import Latte.Grammar.ErrM (Err)
import Latte.Grammar.Par (myLexer, pProgram)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> putStrLn usage >> exitFailure
        [f] -> processFile f
        _ -> syserr "Invalid or no arguments provided"
  where
    usage =
        unlines
            [ "Latte compiler."
            , "Usage: Call with one of the following argument:"
            , "  --help         Display this help message."
            , "  (file)         Run frontend on the file."
            ]

syserr :: String -> IO ()
syserr msg = do
    hPutStrLn stderr "ERROR"
    hPutStrLn stderr msg
    exitFailure

run :: String -> Err ()
run contents = do
    mbCode <- pProgram . myLexer $ contents
    SA.run mbCode

processFile :: String -> IO ()
processFile filename = do
    fileExists <- doesFileExist filename
    unless fileExists (syserr $ "File not found: '" ++ filename ++ "'")
    contents <- readFile filename
    case run contents of
        Left msg -> syserr msg
        Right _ -> hPutStrLn stderr "OK"
