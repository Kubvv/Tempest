module Typecheck.TypeCheckerData where

import Prelude as P
import Data.Map as M
import Syntax.AbsTempest
import BNFC.Abs (BNFC'Position)

---- ChType ----

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

chConvert :: Type BNFC'Position -> EnvType
chConvert (TInt pos) = EnvInt
chConvert (TBool pos) = EnvBool
chConvert (TStr pos) = EnvStr
chConvert (TVoid pos) = EnvVoid
chConvert (TFun pos ret args) = EnvFun (chConvert ret) (P.map chConvert args)

---- Env ----

newtype Env = Env { typeMap :: M.Map Ident EnvType }

gete :: Env -> Ident -> Maybe EnvType
gete env s = M.lookup s (typeMap env)

pute :: Env -> Ident -> EnvType -> Env
pute (Env tMap) s x = Env newtMap
  where newtMap = M.insert s x tMap

initEnv :: Env
initEnv = Env (M.fromList [
    (Ident "printInt", EnvFun EnvVoid [EnvInt]),
    (Ident "printBool", EnvFun EnvVoid [EnvBool]),
    (Ident "printString", EnvFun EnvVoid [EnvStr])
    ])

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

