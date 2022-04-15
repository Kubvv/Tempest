{-# LANGUAGE FlexibleInstances #-}

module Interpret.InterpreterData where

import Data.Map as M
import Data.Maybe

import Syntax.AbsTempest
import BNFC.Abs (BNFC'Position)
import Control.Monad.Except
import Control.Monad.State


---- Exception ----

type InterpretException = InterpretException' BNFC.Abs.BNFC'Position
data InterpretException' a =
  ArithmeticException a
  | ReferenceException a Ident
  | NotImplementedException String

instance Show InterpretException where
  show (ArithmeticException pos) =
    "Division by zero at " ++ showBnfcPos pos
  show (ReferenceException pos id) = concat [
    "Argument passed by reference denoted as ", showIdent id,
    "is not a variable at ", showBnfcPos pos
    ]
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
  freeloc :: Integer,
  returnV :: Value
}

emptyState = State {
  env = M.empty,
  store = M.empty,
  freeloc = 0,
  returnV = VNothing
}

-- State and reader handling
putEnv :: Env -> Mem -> Mem
putEnv env (State _ str fl rv) = State env str fl rv

getLoc :: Ident -> Env -> Loc
getLoc id env = fromJust $ M.lookup id env

getVal :: Loc -> Store -> Value
getVal l str = fromJust $ M.lookup l str

putLoc :: Ident -> Loc -> Env -> Env
putLoc = M.insert

putVal :: Loc -> Value -> Store -> Store
putVal = M.insert

newloc :: Mem -> (Loc, Mem)
newloc (State env str fl rv) = (fl, State env str (fl+1) rv)

getS :: Ident -> Mem -> Value
getS id (State env str _ _) = getVal (getLoc id env) str

putS :: Ident -> Value -> Mem -> Mem
putS id x (State env str fl rv) = State newEnv newStore (fl+1) rv
  where
    newEnv = putLoc id fl env
    newStore = putVal fl x str

updateS :: Ident -> Value -> Mem -> Mem
updateS id x (State env str fl rv) = State env newStore fl rv
  where
    l = getLoc id env
    newStore = putVal l x str

-- Return handling
putReturn :: Value -> Mem -> Mem
putReturn v (State env str fl _) = State env str fl v

getReturn :: Mem -> Value
getReturn (State _ _ _ rv) = rv

isReturn :: Mem -> Bool
isReturn (State _ _ _ VNothing) = False
isReturn _ = True

---- Value ----

data Value = --TODO wymyśleć lepszą nazwe
  VInt Integer
  | VBool Bool
  | VStr String
  | VFun [Arg] Block Env
  | VVoid
  | VNothing

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show (VStr x) = x
  show _ = ""

extractInt :: Value -> Maybe Integer
extractInt (VInt i) = Just i
extractInt _ = Nothing

extractBool :: Value -> Maybe Bool
extractBool (VBool b) = Just b
extractBool _ = Nothing

extractString :: Value -> Maybe String
extractString (VStr s) = Just s
extractString _ = Nothing

extractFun :: Value -> Maybe ([Arg], Block, Env)
extractFun (VFun args block env) = Just (args, block, env)
extractFun _ = Nothing

incrValue :: Value -> Maybe Value
incrValue (VInt i) = Just $ VInt $ i+1
incrValue _ = Nothing 

decrValue :: Value -> Maybe Value
decrValue (VInt i) = Just $ VInt $ i-1
decrValue _ = Nothing 

getFunEnv :: Value -> Maybe Env
getFunEnv (VFun _ _ env) = Just env
getFunEnv _ = Nothing

getFunBlock :: Value -> Maybe Block
getFunBlock (VFun _ b _) = Just b
getFunBlock _ = Nothing

getFunArgs :: Value -> Maybe [Arg]
getFunArgs (VFun args _ _) = Just args
getFunArgs _ = Nothing

---- InterpreterMonad ----

type InterpreterMonad = StateT Mem (ExceptT InterpretException IO) Value

class Interpreter a where

  interpret :: a -> InterpreterMonad