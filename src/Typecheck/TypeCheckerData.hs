{-# LANGUAGE FlexibleInstances    #-}

module Typecheck.TypeCheckerData where

import Prelude as P
import Data.Map as M
import Syntax.AbsTempest
import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Identity (Identity)

---- EnvType ----
-- EnvType is a modified version of Type from AbsTempest, desgined to be the 
-- value of typechecker environment map.

data EnvType =
  EnvInt
  | EnvBool
  | EnvStr
  | EnvVoid
  | EnvFun EnvType [EnvType] [Bool] -- <Return type> <Argument types> <Is argument passed by ref> 

instance Show EnvType where
  show EnvInt = "int"
  show EnvBool = "boolean"
  show EnvStr = "string"
  show EnvVoid = "void"
  show (EnvFun ret args _) = "function from [" ++ show args ++ "] to " ++ show ret

instance Eq EnvType where
  EnvInt == EnvInt = True
  EnvBool == EnvBool = True
  EnvStr == EnvStr = True
  EnvVoid == EnvVoid = True
  (EnvFun ret1 args1 ref1) == (EnvFun ret2 args2 ref2) =
    (length args1 == length args2) &&
    and (zipWith (==) args1 args2) &&
    (length ref1 == length ref2) &&
    and (zipWith (==) ref1 ref2) &&
    (ret1 == ret2)
  _ == _ = False

toEnvType :: Type -> EnvType
toEnvType (TInt pos) = EnvInt
toEnvType (TBool pos) = EnvBool
toEnvType (TStr pos) = EnvStr
toEnvType (TVoid pos) = EnvVoid
toEnvType (TFun pos ret args) = EnvFun (toEnvType ret) (P.map toEnvType args) []

--An easier way of casting a function type to EnvType.
funToEnvType :: Type -> [Arg] -> EnvType
funToEnvType ret args = EnvFun (toEnvType ret) (P.map (toEnvType . getArgType) args) (P.map getArgIsRef args)

--Helper functions used by funToEnvType.
getArgType :: Arg -> Type
getArgType (VArg _ t _) = t
getArgType (RArg _ t _) = t

getArgIsRef :: Arg -> Bool
getArgIsRef VArg {} = False
getArgIsRef RArg {} = True


---- Env ----
-- Environment used by typechecker. typeMap is a map that holds the types of identifiers.
-- expectedReturnType is used by returns in functions to check wheter the return type matches
-- with the declared one. isReturn is used by function declaration to see if there is any return
-- statement used in function's block.

data Env = Env {
  typeMap :: M.Map Ident EnvType,
  expectedReturnType :: Maybe EnvType,
  isReturn :: Bool
}

-- Getters
gete :: Env -> Ident -> Maybe EnvType
gete env s = M.lookup s (typeMap env)

getrt :: Env -> Maybe EnvType
getrt = expectedReturnType

getir :: Env -> Bool
getir = isReturn

pute :: Env -> Ident -> EnvType -> Env
pute (Env tm rt is) id t = Env (M.insert id t tm) rt is

-- Setters
puteArgs :: Env -> [(Ident, EnvType)] -> Env
puteArgs env [] = env
puteArgs env ((id, t):as) = puteArgs newEnv as
  where newEnv = pute env id t

putrt :: Env -> Maybe EnvType -> Env
putrt (Env tm rt is) nt = Env tm nt is

putir :: Env -> Bool -> Env
putir (Env tm rt is) = Env tm rt

-- Starting environment, consisting of default functions (prints, error).
initEnv :: Env
initEnv = Env (M.fromList defaults) Nothing False

defaults :: [(Ident, EnvType)]
defaults = [(Ident "printInt", EnvFun EnvVoid [EnvInt] [False]),
  (Ident "printBool", EnvFun EnvVoid [EnvBool] [False]),
  (Ident "printString", EnvFun EnvVoid [EnvStr] [False]),
  (Ident "error", EnvFun EnvVoid [] [])]

---- Exception ----
-- Exception type is just a nice way of packing all possible typechecker exceptions to one 
-- place. Almost every (besides NoMain) has a position indicating where the exception happened.

data TypeCheckException =
  BadType BNFC'Position EnvType EnvType
  | UnexpectedToken BNFC'Position Ident
  | NotAFunction BNFC'Position EnvType
  | BadArgumentTypes BNFC'Position [EnvType] [EnvType]
  | DuplicateDefinitionsException BNFC'Position
  | ReferenceException BNFC'Position Ident
  | NoReturnException BNFC'Position
  | UnexpectedReturn BNFC'Position
  | NoMainException
  | WrongMainDefinitionException BNFC'Position
  | DuplicateFunctionArgumentsException BNFC'Position
  | DefaultOverrideException BNFC'Position
  | RedefinitionException BNFC'Position Ident
  | VoidVarDefinitionException BNFC'Position

instance Show TypeCheckException where
  show (BadType pos exp act) = concat [
    "Type mismatch at ", showBnfcPos pos,
    ", expected type ", show exp,
    " but got type ", show act
    ]
  show (UnexpectedToken pos id) = concat [
    "Unexpected token at ", showBnfcPos pos,
    ", namely ", showIdent id
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
    "Duplicated definition name at " ++ showBnfcPos pos
  show (ReferenceException pos id) = concat [
    "Expected variable at ", showBnfcPos pos,
    " as this argument is passed by reference to the function ",
    showIdent id
    ]
  show (NoReturnException pos) =
    "No return found for a function at " ++ showBnfcPos pos
  show (UnexpectedReturn pos) =
    "Unexpected return at " ++ showBnfcPos pos
  show NoMainException =
    "No main function found"
  show (WrongMainDefinitionException pos) = concat [
    "Wrong main definition exception at ", showBnfcPos pos,
    ", expected return type of void and no arguments"
    ]
  show (DuplicateFunctionArgumentsException pos) =
    "Two arguments are named the same at " ++ showBnfcPos pos
  show (DefaultOverrideException pos) =
    "Default function overriden at " ++ showBnfcPos pos
  show (RedefinitionException pos id) = concat [
    "Redefinition of identifier ", showIdent id, " at ", showBnfcPos pos
    ]
  show (VoidVarDefinitionException pos) =
    "Variable has type void at " ++ showBnfcPos pos

-- Shows for unordinary types.
showBnfcPos :: BNFC'Position -> String
showBnfcPos (Just (r, c)) = concat [
  "line ", show r,
  ", position ", show c
  ]
showBnfcPos Nothing = "error while printing the exception position"

showIdent :: Ident -> String
showIdent (Ident s) = s

---- CheckerMonad ----
-- CheckerMonad is used in evaluating all syntax types, besides the expr.
-- Because of many data types that uses this monad, it also has a class associated with it.

type CheckerMonad = StateT Env (ExceptT TypeCheckException Identity) ()

class Checker a where
  checkType :: a -> CheckerMonad

---- GetterMonad ----
-- Getter monad is used solely by expr syntax type.

type GetterMonad = ReaderT Env (ExceptT TypeCheckException Identity)