{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Control.Monad.State

compareType :: BNFC'Position -> EnvType -> EnvType -> EmptyGetterMonad
compareType pos et at =
  do
    when (at /= et) $
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

compareTypeExpr :: BNFC'Position -> EnvType -> Expr BNFC'Position -> EmptyGetterMonad
compareTypeExpr pos et e =
  do
    at <- getType e
    when (at /= et) $
      throwError (BadType pos et at)

runComparator :: BNFC'Position -> EnvType -> Expr BNFC'Position -> Env -> Either TypeCheckException ()
runComparator pos ext e env = runExcept $ runReaderT (compareTypeExpr pos ext e) env

checkResult :: Either TypeCheckException () -> CheckerMonad
checkResult et = case et of
  Right _ -> return ()
  Left ex -> throwError ex

checkCondition :: Bool -> TypeCheckException -> CheckerMonad
checkCondition b ex =
  do
    unless b $
      throwError ex

checkMain :: Maybe (Def BNFC'Position) -> CheckerMonad
checkMain Nothing = throwError NoMainException
checkMain (Just (FnDef pos rt _ args _)) = do
  let ret = toEnvType rt
  checkCondition (ret == EnvVoid && P.null args) (WrongMainDefinitionException pos)
checkMain (Just _) = return ()


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


instance Checker (Program BNFC'Position) where
  checkType (PProgram pos defs) =
    do
      unless (uniqueDefs defs) $
        throwError (DuplicateDefinitionsException pos)
      mapM_ checkType defs
      checkMain $ findByIdent defs

instance Checker (Def BNFC'Position) where
  checkType (GlDef pos t id e) =
    do
      env <- get
      let ext = toEnvType t
      let result = runComparator pos ext e env
      checkResult result
      put (pute env id ext)

  checkType (FnDef pos rt id args block) =
    do
      env <- get
      put $ pute env id (funToEnvType rt args)
      newEnv <- get
      put $ puteArgs newEnv (argTypes args)
      beforeEnv <- get
      put $ putrt beforeEnv (Just (toEnvType rt))
      checkType block
      afterEnv <- get
      checkCondition (isReturn afterEnv) (NoReturnException pos)
      put newEnv

instance Checker (Block BNFC'Position) where
  checkType (BBlock pos stmts) = mapM_ checkType stmts

instance Checker (Stmt BNFC'Position) where
  --Empty statement
  checkType (SEmpty _) = return ()

  --Block
  checkType (SBStmt _ block) =
    do
      env <- get
      checkType block
      put env

  --Init and Assignment
  checkType (SInit _ def) = checkType def

  checkType (SAss pos id e) =
    do
      env <- get
      case gete env id of
        Just et -> checkResult $ runComparator pos et e env
        Nothing -> throwError (UnexpectedToken pos id)

  --Increment and Decrement
  checkType (SIncr pos id) =
    do
      env <- get
      case gete env id of
        Just at -> checkCondition (at == EnvInt) (BadType pos EnvInt at)
        Nothing -> throwError (UnexpectedToken pos id)

  checkType (SDecr pos id) =
    do
      env <- get
      case gete env id of
        Just at -> checkCondition (at == EnvInt) (BadType pos EnvInt at)
        Nothing -> throwError (UnexpectedToken pos id)

  --Returns
  checkType (SRet pos e) =
    do
      env <- get
      case expectedReturnType env of
        Just et -> checkResult $ runComparator pos et e env
        Nothing -> throwError (UnexpectedReturn pos)
      put $ putir env True

  checkType (SVRet pos) =
    do
      env <- get
      case expectedReturnType env of
        Just et -> checkCondition (et == EnvVoid) (BadType pos EnvVoid et)
        Nothing -> throwError (UnexpectedReturn pos)
      put $ putir env True

  --Conditionals
  checkType (SCond pos cond block) =
    do
      env <- get
      checkResult $ runComparator pos EnvBool cond env
      checkType block

  checkType (SCondElse pos cond block1 block2) =
    do
      env <- get
      checkResult $ runComparator pos EnvBool cond env
      checkType block1
      checkType block2

  --While
  checkType (SWhile pos cond block) =
    do
      env <- get
      checkResult $ runComparator pos EnvBool cond env
      checkType block

  --Expression
  checkType (SExp pos e) =
    do
      env <- get
      checkResult $ runComparator pos EnvVoid e env
