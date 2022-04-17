{-# LANGUAGE FlexibleInstances #-}

module Interpret.InterpreterData where

import Data.Map as M
import Data.Maybe

import Syntax.AbsTempest
import Control.Monad.Except
import Control.Monad.State


---- Exception ----
-- Exception type is just a nice way of packing all possible interpreter exceptions to one 
-- place. All of them have a position indicating where given exception happened.

data InterpretException =
  ArithmeticException BNFC'Position 
  | NotImplementedException BNFC'Position String
  | ErrorMethodCalledException BNFC'Position
  | NoReturnEncounteredException BNFC'Position Ident

instance Show InterpretException where
  show (ArithmeticException pos) =
    "Division by zero at " ++ showBnfcPos pos
  show (NotImplementedException pos s) = concat [
    "Function ", s, 
    " at ", showBnfcPos pos,
    " is declared as default, but it doesn't have a default implementation"
    ]
  show (NoReturnEncounteredException pos id) = concat [
    "No return encountered for function ", showIdent id,
    " called at ", showBnfcPos pos
    ]
  show (ErrorMethodCalledException pos) =
    "Error method was called at " ++ showBnfcPos pos

-- Shows for unordinary types.
showBnfcPos :: BNFC'Position -> String
showBnfcPos (Just (r, c)) = concat [
  "line ", show r,
  ", position ", show c
  ]
showBnfcPos Nothing = "error while printing the exception position"

showIdent :: Ident -> String
showIdent (Ident s) = s

---- Interpreter's data (Mem) ----
-- Mem is used by interpreter for storing data. It uses a special type Loc, which is a
-- bridge between identifier and a result. Loc introduction allows for detaching the environment,
-- while keeping the state of the store. freeloc is used internally to keep track of the smallest
-- location number that is free. returnR decribes the result value of a return, which is later used
-- by function application to return this value. When returnR != RNothing, that is an indicator that
-- return occured and that next statements up to reaching the end of the function should be ignored.

type Loc = Integer
type Env = M.Map Ident Loc
type Store = M.Map Loc Result

data Mem = State {
  env :: Env,
  store :: Store,
  freeloc :: Integer,
  returnR :: Result
}

emptyState = State {
  env = M.empty,
  store = M.empty,
  freeloc = 0,
  returnR = RNothing
}

-- State handling
getLoc :: Ident -> Env -> Loc
getLoc id env = fromJust $ M.lookup id env

getRes :: Loc -> Store -> Result
getRes l str = fromJust $ M.lookup l str

putEnv :: Env -> Mem -> Mem
putEnv envi (State _ str fl rv) = State envi str fl rv

putLoc :: Ident -> Loc -> Env -> Env
putLoc = M.insert

putRes :: Loc -> Result -> Store -> Store
putRes = M.insert

newloc :: Mem -> (Loc, Mem)
newloc (State env str fl rv) = (fl, State env str (fl+1) rv)

-- Getter, putter and updater for whole state
getS :: Ident -> Mem -> Result
getS id (State env str _ _) = getRes (getLoc id env) str

putS :: Ident -> Result -> Mem -> Mem
putS id x (State env str fl rv) = State newEnv newStore (fl+1) rv
  where
    newEnv = putLoc id fl env
    newStore = putRes fl x str

updateS :: Ident -> Result -> Mem -> Mem
updateS id x (State env str fl rv) = State env newStore fl rv
  where
    l = getLoc id env
    newStore = putRes l x str

-- Return handling
putReturn :: Result -> Mem -> Mem
putReturn v (State env str fl _) = State env str fl v

getReturn :: Mem -> Result
getReturn (State _ _ _ rv) = rv

isReturn :: Mem -> Bool
isReturn (State _ _ _ RNothing) = False
isReturn _ = True

---- Result ----
-- Result is a modified Type from AbsTempest, desgined to be the 
-- value of interpreter environment map.

data Result =
  RInt Integer
  | RBool Bool
  | RStr String
  | RFun [Arg] Block Env
  | RVoid
  | RNothing

instance Show Result where
  show (RInt x) = show x
  show (RBool x) = show x
  show (RStr x) = x
  show (RFun args block env) = "Function"
  show RVoid = "Void"
  show RNothing = "Nothing"

--Unpack the result to get values inside
extractInt :: Result -> Maybe Integer
extractInt (RInt i) = Just i
extractInt _ = Nothing

extractBool :: Result -> Maybe Bool
extractBool (RBool b) = Just b
extractBool _ = Nothing

extractString :: Result -> Maybe String
extractString (RStr s) = Just s
extractString _ = Nothing

extractFun :: Result -> Maybe ([Arg], Block, Env)
extractFun (RFun args block env) = Just (args, block, env)
extractFun _ = Nothing

--Increment/decrement integer by 1 or do nothing
incrResult :: Result -> Maybe Result
incrResult (RInt i) = Just $ RInt $ i+1
incrResult _ = Nothing 

decrResult :: Result -> Maybe Result
decrResult (RInt i) = Just $ RInt $ i-1
decrResult _ = Nothing

---- InterpreterMonad ----
-- InterpreterMonad is used in evaluating all syntax types
-- Because of many data types that uses this monad, it also has a class associated with it.

type InterpreterMonad = StateT Mem (ExceptT InterpretException IO) Result

class Interpreter a where

  interpret :: a -> InterpreterMonad