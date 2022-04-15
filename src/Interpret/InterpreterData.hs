{-# LANGUAGE FlexibleInstances #-}

module Interpret.InterpreterData where

import Data.Map as M
import Data.Maybe

import Syntax.AbsTempest
import Control.Monad.Except
import Control.Monad.State


---- Exception ----

data InterpretException =
  ArithmeticException BNFC'Position 
  | ReferenceException BNFC'Position Ident
  | NotImplementedException String
  | NoReturnEncounteredException BNFC'Position Ident

instance Show InterpretException where
  show (ArithmeticException pos) =
    "Division by zero at " ++ showBnfcPos pos
  show (ReferenceException pos id) = concat [
    "Argument passed by reference denoted as ", showIdent id,
    " is not a variable at ", showBnfcPos pos
    ]
  show (NotImplementedException id) = concat [
    "Function ", id,
    " is declared as default, but it doesn't have a default implementation"
    ]
  show (NoReturnEncounteredException pos id) = concat [
    "No return encountered for function ", showIdent id,
    " called at ", showBnfcPos pos
    ]

showBnfcPos :: BNFC'Position -> String
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

-- State and reader handling
putEnv :: Env -> Mem -> Mem
putEnv envi (State _ str fl rv) = State envi str fl rv

getLoc :: Ident -> Env -> Loc
getLoc id env = fromJust $ M.lookup id env

getRes :: Loc -> Store -> Result
getRes l str = fromJust $ M.lookup l str

putLoc :: Ident -> Loc -> Env -> Env
putLoc = M.insert

putRes :: Loc -> Result -> Store -> Store
putRes = M.insert

newloc :: Mem -> (Loc, Mem)
newloc (State env str fl rv) = (fl, State env str (fl+1) rv)

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
  show _ = ""

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

type InterpreterMonad = StateT Mem (ExceptT InterpretException IO) Result

class Interpreter a where

  interpret :: a -> InterpreterMonad