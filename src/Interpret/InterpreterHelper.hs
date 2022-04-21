{-# LANGUAGE FlexibleInstances #-}
module Interpret.InterpreterHelper where

import Prelude as P
import Data.Maybe
import Interpret.InterpreterData
import Syntax.AbsTempest
import Control.Monad.Except
import Control.Monad.State

---- Default functions ----

prints :: [[Char]]
prints = ["printString", "printInt", "printBool"]

errors :: [[Char]]
errors = ["error"]

defaults :: [[Char]]
defaults = prints ++ errors

interpretDefault :: BNFC'Position -> Ident -> [Result]-> InterpreterMonad
interpretDefault pos id@(Ident s) args
  | s `elem` prints = interpretDefaultPrint $ head args
  | s `elem` errors = interpretDefaultError pos
  | otherwise = throwError $ NotImplementedException pos id

interpretDefaultPrint :: Result -> InterpreterMonad
interpretDefaultPrint x =
  do
    liftIO $ print x
    return RNothing

interpretDefaultError :: BNFC'Position -> InterpreterMonad
interpretDefaultError pos =
  throwError $ ErrorMethodCalledException pos

---- Evaluation ----

isVariable :: Expr -> Bool
isVariable (EVar _ _) = True
isVariable _ = False

-- Getter for syntax operators.
class ArithmeticOperator a where

  getArthOperator :: a -> (Integer -> Integer -> Integer)

instance ArithmeticOperator AddOp where

  getArthOperator (OAdd _) = (+)
  getArthOperator (OSub _) = (-)

instance ArithmeticOperator MulOp where

  getArthOperator (OMul _) = (*)
  getArthOperator (ODiv _) = div
  getArthOperator (OMod _) = mod

getCompOperator :: RelOp -> (Integer -> Integer -> Bool)
getCompOperator (OLs _) = (<)
getCompOperator (OLe _) = (<=)
getCompOperator (OGr _) = (>)
getCompOperator (OGe _) = (>=)
getCompOperator (OEq _) = (==)
getCompOperator (ONe _) = (/=)

isDiv :: MulOp -> Bool
isDiv (ODiv _) = True
isDiv _ = False

getConOperator :: ConOp -> (String -> String -> String)
getConOperator (OCon _) = (++)

-- Get the location of passed argument (function application).
getArgLoc :: Env -> Expr -> Maybe Loc
getArgLoc env (EVar _ id) = Just (getLoc id env)
getArgLoc _ _ = Nothing

-- Modifies the environment so that it is correctly wired with all arguments,
-- both the one passed by value and reference.
putArgs :: [Result] -> [Maybe Loc] -> [Arg] -> InterpreterMonad
putArgs [] _ _ = return RNothing
putArgs (r:rs) (l:ls) (a:as) =
  do
    putArg r l a
    putArgs rs ls as
putArgs _ _ _ = return RNothing

putArg :: Result -> Maybe Loc -> Arg -> InterpreterMonad
putArg _ ml (RArg _ _ id) =
  do
    mem <- get
    let envi = env mem
    let l = fromJust ml
    let newEnvi = putLoc id l envi
    put $ putEnv newEnvi mem
    return RNothing

putArg r _ (VArg _ _ id) =
  do
    mem <- get
    put $ putS id r mem
    return RNothing

-- Finds the main function and returns its block of statements.
findMainBlock :: [Def] -> Maybe Block
findMainBlock [] = Nothing
findMainBlock ((FnDef _ _ (Ident "main") _ b):ds) = Just b
findMainBlock (d:ds) = findMainBlock ds
