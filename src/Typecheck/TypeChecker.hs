{-# LANGUAGE BlockArguments #-}
module Typecheck.TypeChecker where

import Prelude as P
import Data.Map as M

import Typecheck.TypeCheckerData
import Typecheck.TypeCheckerHelper
import Syntax.AbsTempest
import BNFC.Abs (BNFC'Position)


getType :: Expr BNFC'Position -> GetterMonad
--Trivial
getType (ELitFalse _) = return EnvBool
getType (ELitTrue _) = return EnvBool
getType (ELitInt _ _) = return EnvInt
getType (EString _ _) = return EnvStr

--Expect bool
getType (ENot pos e) = 
  do
    evType <- getType e 
    if (evType == EnvBool) 
      then return EnvBool
    else
      throwError (BadType pos EnvBool evType)

getType (EAnd pos e1 _ e2) = 
  do

getType (EOr pos e1 _ e2) = 
  do

--Except Int
getType (ENeg pos e) = 
  do
    evType <- getType e 
    if (evType == EnvInt) 
      then return EnvInt
    else
      throwError (BadType pos EnvInt evType)

getType (EMul pos e1 _ e2) = 
  do

getType (EAdd pos e1 _ e2) = 
  do

getType (ERel pos e1 _ e2) = 
  do

checkType :: BNFC'Position -> Expr -> EnvType
checkType pos e et = 