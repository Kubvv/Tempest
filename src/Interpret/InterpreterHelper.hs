{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# LANGUAGE FlexibleInstances #-}
module Interpret.InterpreterHelper where

import Prelude as P
import Interpret.InterpreterData
import Syntax.AbsTempest
import Control.Monad.Except
import Control.Monad.State

---- Default functions ----

prints = ["printString", "printInt", "printBool"]
defaults = prints

interpretDefault :: Ident -> [Value]-> InterpreterMonad
interpretDefault (Ident s) args =
  if elem s prints && length args == 1 then
    interpretDefaultPrint $ head args
  else
    throwError $ NotImplementedException s

interpretDefaultPrint :: Value -> InterpreterMonad
interpretDefaultPrint x =
  do
    liftIO $ print x
    return VNothing

---- Evaluation ----

isVariable :: Expr -> Bool
isVariable (EVar _ _) = True
isVariable _ = False

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

getArgLoc :: Env -> Expr -> Maybe Loc
getArgLoc env (EVar _ id) = Just (getLoc id env)
getArgLoc _ _ = Nothing

putArgs :: [Value] -> [Maybe Loc] -> [Arg] -> InterpreterMonad 
putArgs [] _ _ = return VNothing
putArgs (v:vs) (l:ls) (a:as) = 
  do
    putArg v l a
    putArgs vs ls as
putArgs _ _ _ = return VNothing 

putArg :: Value -> Maybe Loc -> Arg -> InterpreterMonad 

putArg _ Nothing (RArg pos _ id) =
  throwError $ ReferenceException pos id

putArg _ (Just l) (RArg _ _ id) =
  do
    mem <- get
    let envi = env mem
    let newEnvi = putLoc id l envi
    put $ putEnv newEnvi mem
    return VNothing

putArg v _ (VArg _ _ id) =
  do
    mem <- get
    put $ putS id v mem
    return VNothing

interpretIfNotRet :: InterpreterMonad -> InterpreterMonad
interpretIfNotRet inter = do
  mem <- get
  let isRet = isReturn mem
  if isRet then
    inter
  else
    return VNothing 
