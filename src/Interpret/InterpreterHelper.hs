{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
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
interpretDefault (Ident s) xs =
  if elem s prints && length xs == 1 then
    interpretDefaultPrint $ head xs
  else
    throwError $ NotImplementedException s

interpretDefaultPrint :: Value -> InterpreterMonad
interpretDefaultPrint x =
  do
    liftIO $ print x
    return VNothing
