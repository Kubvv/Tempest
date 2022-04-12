{-# LANGUAGE FlexibleInstances    #-}

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

toEnvType :: Type -> EnvType
toEnvType (TInt pos) = EnvInt
toEnvType (TBool pos) = EnvBool
toEnvType (TStr pos) = EnvStr
toEnvType (TVoid pos) = EnvVoid
toEnvType (TFun pos ret args) = EnvFun (toEnvType ret) (P.map toEnvType args)

funToEnvType :: Type -> [Arg] -> EnvType
funToEnvType ret args = EnvFun (toEnvType ret) (P.map (toEnvType . getArgType) args)

getArgType :: Arg -> Type
getArgType (VArg _ t _) = t
getArgType (RArg _ t _) = t

---- Env ----

data Env = Env {
  typeMap :: M.Map Ident EnvType,
  expectedReturnType :: Maybe EnvType,
  isReturn :: Bool
}

gete :: Env -> Ident -> Maybe EnvType
gete env s = M.lookup s (typeMap env)

getrt :: Env -> Maybe EnvType
getrt = expectedReturnType

getir :: Env -> Bool
getir = isReturn

pute :: Env -> Ident -> EnvType -> Env
pute (Env tm rt is) id t = Env (M.insert id t tm) rt is

puteArgs :: Env -> [(Ident, EnvType)] -> Env
puteArgs env [] = env
puteArgs env ((id, t):as) = puteArgs newEnv as
  where newEnv = pute env id t

putrt :: Env -> Maybe EnvType -> Env
putrt (Env tm rt is) nt = Env tm nt is

putir :: Env -> Bool -> Env
putir (Env tm rt is) = Env tm rt

initEnv :: Env
initEnv = Env (M.fromList [
  (Ident "printInt", EnvFun EnvVoid [EnvInt]),
  (Ident "printBool", EnvFun EnvVoid [EnvBool]),
  (Ident "printString", EnvFun EnvVoid [EnvStr])
  ]) Nothing False

---- Exception ----

showBnfcPos :: BNFC.Abs.BNFC'Position -> String
showBnfcPos (Just (r, c)) = concat [
  "line ", show r,
  ", position ", show c
  ]
showBnfcPos Nothing = "error while printing the exception position"

type TypeCheckException = TypeCheckException' BNFC.Abs.BNFC'Position 
data TypeCheckException' a =
  BadType a EnvType EnvType
  | UnexpectedToken a Ident
  | NotAFunction a EnvType
  | BadArgumentTypes a [EnvType] [EnvType]
  | DuplicateDefinitionsException a
  | NoReturnException a
  | UnexpectedReturn a
  | NoMainException
  | WrongMainDefinitionException a

instance Show TypeCheckException where
  show (BadType pos exp act) = concat [
    "Type mismatch at ", showBnfcPos pos,
    ", expeceted type ", show exp,
    " but got: ", show act
    ]
  show (UnexpectedToken pos id) = concat [
    "Unexpected token at ", showBnfcPos pos,
    ", namely ", show id
    ]
  show (NotAFunction pos act) = concat [
    "Expected a function at ", showBnfcPos pos,
    ", but got type ", show act
    ]
  show (BadArgumentTypes pos exp act) = concat [
    "Wrong argument types at ", showBnfcPos pos,
    ", expected types ", show exp,
    ", but got types ", show act
    ]
  show (DuplicateDefinitionsException pos) =
    "Two definitons are named the same at " ++ showBnfcPos pos
  show (NoReturnException pos) =
    "No return found for function at " ++ showBnfcPos pos
  show (UnexpectedReturn pos) =
    "Unexpected return at " ++ showBnfcPos pos
  show NoMainException =
    "No main function found"
  show (WrongMainDefinitionException pos) = concat [
    "Wrong main definition exception at ", showBnfcPos pos,
    "Expected return type of void and no arguments"
    ]

---- CheckerMonad ----
type CheckerMonad = StateT Env (ExceptT TypeCheckException Identity) ()

class Checker a where
  checkType :: a -> CheckerMonad

---- GetterMonad ----
type GetterMonad = ReaderT Env (ExceptT TypeCheckException Identity) EnvType
type EmptyGetterMonad = ReaderT Env (ExceptT TypeCheckException Identity) ()