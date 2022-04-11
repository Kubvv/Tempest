{-# LANGUAGE BlockArguments #-}
module Typecheck.TypeChecker where

import Prelude as P
import Data.Map as M

import Typecheck.TypeCheckerData
import Typecheck.TypeCheckerHelper
import Syntax.AbsTempest
import BNFC.Abs (BNFC'Position)
import Control.Monad as CM
import Control.Monad.Except
import Control.Monad.Reader



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
    compareType pos EnvBool evType
    return EnvBool

getType (EAnd pos e1 e2) =
  do
    evType1 <- getType e1
    evType2 <- getType e2
    compareTypes pos EnvBool evType1 evType2
    return EnvBool

getType (EOr pos e1 e2) =
  do
    evType1 <- getType e1
    evType2 <- getType e2
    compareTypes pos EnvBool evType1 evType2
    return EnvBool

--Expect int
getType (ENeg pos e) =
  do
    evType <- getType e
    compareType pos EnvInt evType
    return EnvInt

getType (EMul pos e1 _ e2) =
  do
    evType1 <- getType e1
    evType2 <- getType e2
    compareTypes pos EnvInt evType1 evType2
    return EnvInt

getType (EAdd pos e1 _ e2) =
  do
    evType1 <- getType e1
    evType2 <- getType e2
    compareTypes pos EnvInt evType1 evType2
    return EnvInt

getType (ERel pos e1 _ e2) =
  do
    evType1 <- getType e1
    evType2 <- getType e2
    compareTypes pos EnvInt evType1 evType2
    return EnvInt

--Var and Function
getType (EVar pos id) =
  do
    env <- ask
    case gete env id of
      Just t -> return t
      Nothing -> throwError (UnexpectedToken pos id)

getType (EApp pos id args) =
  do
    env <- ask
    case gete env id of
      Just t -> checkFunction pos t args
      Nothing -> throwError (UnexpectedToken pos id)


compareType :: BNFC'Position -> EnvType -> EnvType -> EmptyGetterMonad
compareType pos et at = 
  do
    CM.when (at /= et) $
      throwError (BadType pos et at)

compareTypes :: BNFC'Position -> EnvType -> EnvType -> EnvType -> EmptyGetterMonad
compareTypes pos et at1 at2 = 
  do
    compareType pos et at1
    compareType pos et at2

checkFunction :: BNFC'Position -> EnvType -> [Expr BNFC'Position] -> GetterMonad
checkFunction pos (EnvFun ret exTypes) args = 
  do
    acTypes <- mapM getType args
    if and (zipWith (==) exTypes acTypes) then
      return ret
    else
      throwError (BadArgumentTypes pos exTypes acTypes)

checkFunction pos t _ =
  throwError (NotAFunction pos t)


