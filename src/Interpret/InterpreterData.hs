{-# LANGUAGE FlexibleInstances #-}

module Interpret.InterpreterData where

import Data.Map as M
import Data.Maybe

import Syntax.AbsTempest
import BNFC.Abs (BNFC'Position)
import           Control.Monad.Except
import           Control.Monad.State



---- Exception ----

type InterpretException = InterpretException' BNFC.Abs.BNFC'Position
data InterpretException' a =
  ArithmeticException a
  | ReferenceException a
  | NotImplementedException String

instance Show InterpretException where
  show (ArithmeticException pos) =
    "Division by zero at " ++ showBnfcPos pos
  show (ReferenceException pos) =
    "Argument passed by reference is not a variable at " ++ showBnfcPos pos
  show (NotImplementedException id) = concat [
    "Function ", id,
    " is declared as default, but it doesn't have a default implementation"
    ]

showBnfcPos :: BNFC.Abs.BNFC'Position -> String
showBnfcPos (Just (r, c)) = concat [
  "line ", show r,
  ", position ", show c
  ]
showBnfcPos Nothing = "error while printing the exception position"

showIdent :: Ident -> String
showIdent (Ident s) = s

---- Interpreter's data ----

type Loc = Integer
type Env = M.Map Ident Loc
type Store = M.Map Loc Value

data Mem = State {
  env :: Env,
  store :: Store,
  freeloc :: Integer
}

emptyState = State {
  env = M.empty,
  store = M.empty,
  freeloc = 0
}

putEnv :: Env -> Mem -> Mem
putEnv env (State _ str fl) = State env str fl

getLoc :: Ident -> Env -> Loc
getLoc id env = fromJust $ M.lookup id env

getVal :: Loc -> Store -> Value
getVal l str = fromJust $ M.lookup l str

putLoc :: Ident -> Loc -> Env -> Env
putLoc = M.insert

putVal :: Loc -> Value -> Store -> Store
putVal = M.insert

newloc :: Mem -> (Loc, Mem)
newloc (State env str fl) = (fl, State env str (fl+1))

get :: Ident -> Mem -> Value
get id (State env str _) = getVal (getLoc id env) str

put :: Ident -> Value -> Mem -> Mem
put id x (State env str fl) = State newEnv newStore (fl+1)
  where
    newEnv = putLoc id fl env
    newStore = putVal fl x str

update :: Ident -> Value -> Mem -> Mem
update id x (State env str fl) = State env newStore fl
  where
    l = getLoc id env
    newStore = putVal l x str

---- Value ----

data Value =
  VInt Integer
  | VBool Bool
  | VString String
  | VFun [Arg] Block Env
  | VNothing

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show (VString x) = x
  show _ = ""


---- InterpreterMonad ----

type InterpreterMonad = StateT Mem (ExceptT InterpretException IO) Value

class Interpreter a where

  interpret :: a -> InterpreterMonad