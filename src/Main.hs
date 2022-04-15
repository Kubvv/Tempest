module Main where

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Syntax.AbsTempest
import Syntax.ParTempest ( pProgram, myLexer )
import Typecheck.TypeChecker
import Interpret.Interpreter
import BNFC.Abs (BNFC'Position)

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
    Right t -> checkTypes t
    Left err -> exitErrWithMessage err

checkTypes :: Program -> IO ()
checkTypes t =
  case runTypeCheck t of
    Right _ -> checkInterpret t
    Left err -> exitErrWithMessage $ show err
  
checkInterpret :: Program -> IO ()
checkInterpret t = do
  result <- runInterpreter t
  case result of
    Right _ -> exitSuccess
    Left err -> exitErrWithMessage $ show err
