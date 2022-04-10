module Typecheck.TypeCheckerData where

import Prelude as P
import Data.Map as M
import Syntax.AbsTempest
import BNFC.Abs (BNFC'Position)
import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Identity (Identity)

---- EnvType ----

data EnvType = EnvInt
  | EnvBool
  | EnvStr
  | EnvVoid
  | EnvFun EnvType [EnvType]

instance Show EnvType where
  show EnvInt = "int"
  show EnvBool = "boolean"
  show EnvStr = "string"
  show EnvVoid = "void"
  show (EnvFun ret args) = "function from [" ++ show args ++ "] to " ++ show ret

instance Eq EnvType where
  EnvInt == EnvInt = True
  EnvBool == EnvBool = True
  EnvStr == EnvStr = True
  (EnvFun ret1 args1) == (EnvFun ret2 args2) = and (zipWith (==) args1 args2) && (ret1 == ret2)
  _ == _ = False


toEnvType :: Type BNFC'Position -> EnvType
toEnvType (TInt pos) = EnvInt
toEnvType (TBool pos) = EnvBool
toEnvType (TStr pos) = EnvStr
toEnvType (TVoid pos) = EnvVoid
toEnvType (TFun pos ret args) = EnvFun (toEnvType ret) (P.map toEnvType args)

---- Env ----

type Env = M.Map Ident EnvType

gete :: Env -> Ident -> Maybe EnvType
gete env s = M.lookup s env

pute :: Env -> Ident -> EnvType -> Env
pute env s x = M.insert s x env

initEnv :: Env
initEnv = M.fromList [
  (Ident "printInt", EnvFun EnvVoid [EnvInt]),
  (Ident "printBool", EnvFun EnvVoid [EnvBool]),
  (Ident "printString", EnvFun EnvVoid [EnvStr])
  ]

---- Exception ----

showBnfcPos :: BNFC'Position -> String
showBnfcPos (Just (r, c)) = concat [
  "line ", show r,
  ", position ", show c
  ]
showBnfcPos Nothing = "error while printing the exception position"

data TypeCheckException =
  BadType BNFC'Position EnvType EnvType
  | UnexpectedToken BNFC'Position Ident

instance Show TypeCheckException where
  show (BadType pos exp act) = concat [
    "Type mismatch at ", showBnfcPos pos,
    ": Expeceted type ", show exp,
    " but got: ", show act
    ]
  show (UnexpectedToken pos id) = concat [
    "Unexpected token at ", showBnfcPos pos,
    ", namely ", show id
    ]

---- CheckerMonad ----
type CheckerMonad = StateT Env (ExceptT TypeCheckException Identity) ()

---- GetterMonad ----
type GetterMonad = ReaderT Env (ExceptT TypeCheckException Identity) EnvType
type EmptyGetterMonad = ReaderT Env (ExceptT TypeCheckException Identity) ()