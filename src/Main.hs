module Main where

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Directory.Internal.Prelude (exitFailure)
import Syntax.ParTempest ( pProgram, myLexer )
import Syntax.ErrM ( Err(Ok, Bad) )

main :: IO ()
main = 
  do
    args <- getArgs
    checkArgsCount args 

checkArgsCount :: [String] -> IO ()
checkArgsCount [] = 
  do
    conts <- getContents
    checkSyntax conts
checkArgsCount [x] =
  do 
    conts <- readFile x
    checkSyntax conts
checkArgsCount _ = exitErr

exitErr :: IO ()
exitErr =
  do
    putStrLn "Wrong number of arguments"
    putStrLn "Pass 0 arguments to interpret Tempest program from stdin"
    putStrLn "Pass 1 argument to interpret the Tempest file given as argument"
    exitFailure

exitErrWithMessage :: String -> IO ()
exitErrWithMessage err =
  do
    hPutStrLn stderr err
    exitFailure
      
checkSyntax :: String -> IO ()
checkSyntax input = 
  case pProgram $ myLexer input of
    Bad err -> exitErrWithMessage err
    Ok t -> exitSuccess --interpret t
  
-- interpret :: Program -> IO ()
-- interpret t =
--   case typeCheck t of
--     Left err -> exitErrWithMessage err
--     Right _ -> exitSuccess
