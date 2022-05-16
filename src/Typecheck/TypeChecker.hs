{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
module Typecheck.TypeChecker where

import Prelude as P
import Data.Map as M
import Data.Set as S

import Typecheck.TypeCheckerData
import Typecheck.TypeCheckerHelper
import Syntax.AbsTempest
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

--Basic checks.
compareType :: BNFC'Position -> EnvType -> EnvType -> GetterMonad ()
compareType pos et at =
  do
    when (at /= et) $
      throwError (BadType pos et at)

compareTypes :: BNFC'Position -> EnvType -> EnvType -> EnvType -> GetterMonad ()
compareTypes pos et at1 at2 =
  do
    compareType pos et at1
    compareType pos et at2

checkResult :: Either TypeCheckException () -> CheckerMonad
checkResult et = case et of
  Right _ -> return ()
  Left ex -> throwError ex

checkCondition :: Bool -> TypeCheckException -> CheckerMonad
checkCondition b ex =
  do
    unless b $
      throwError ex

--Checks if all arguments that are passed by reference are variables.
--If not checkRefs throws an appropriate exception.
checkRefs :: [Bool] -> [Expr] -> Ident -> GetterMonad ()
checkRefs [] _ _ = return ()
checkRefs _ [] _ = return ()
checkRefs (_:bs) (EVar {}:es) id = checkRefs bs es id
checkRefs (b:bs) (e:es) id = 
  if b then
    throwError (ReferenceException (exprToPos e) id)
  else
    checkRefs bs es id

--Checks if given function application matches the definition, aka
--it checks the number of args, their types and reference passes.
checkFunction :: BNFC'Position -> Ident -> EnvType -> [Expr] -> GetterMonad EnvType 
checkFunction pos id (EnvFun ret exTypes ref) args =
  do
    acTypes <- mapM getType args
    let comparedArgs = zipWith (==) exTypes acTypes
    if and comparedArgs && (length acTypes == length exTypes) then
      checkRefs ref args id >>
      return ret
    else
      throwError (BadArgumentTypes pos exTypes acTypes)

checkFunction pos _ t _ =
  throwError (NotAFunction pos t)
 
--Runs comparator for an expected type (EnvType) and actual type (which is evaluated from expr).
runComparator :: BNFC'Position -> EnvType -> Expr -> Env -> Either TypeCheckException ()
runComparator pos ext e env = runExcept $ runReaderT (compareTypeExpr pos ext e) env

compareTypeExpr :: BNFC'Position -> EnvType -> Expr -> GetterMonad ()
compareTypeExpr pos et e =
  do
    at <- getType e
    when (at /= et) $
      throwError (BadType pos et at)

--Check if main exists and is a zero argument void function.
checkMain :: Maybe Def -> CheckerMonad
checkMain Nothing = throwError NoMainException
checkMain (Just (FnDef pos rt _ args _)) = do
  let ret = toEnvType rt
  checkCondition (ret == EnvVoid && P.null args) (WrongMainDefinitionException pos)
checkMain (Just _) = return ()

--Checks for dupliactes in definition naming.
checkDefs :: Int -> [Def] -> Set Ident -> CheckerMonad
checkDefs _ [] _ = return ()
checkDefs es (d:ds) set = if es == S.size newSet then
    checkDefs (es + 1) ds newSet
  else
    throwError (DuplicateDefinitionsException (defToPos d))
  where
    newSet = S.insert (defToIdent d) set

--Checks if some variable is defined twice in the same block.
checkInitsRedef :: [Stmt] -> Set Ident -> CheckerMonad
checkInitsRedef [] _ = return ()
checkInitsRedef ((SInit pos def):stmts) set = 
  do
    let id = defToIdent def
    if S.member id set then
      throwError (RedefinitionException pos id)
    else
      checkInitsRedef stmts (S.insert id set)
checkInitsRedef (_:stmts) set = checkInitsRedef stmts set

--Checks if all arguments have correct type (int, bool or string).
areCorrectArgTypes :: [Arg] -> CheckerMonad
areCorrectArgTypes [] = return ()
areCorrectArgTypes (a:as) = 
  if correctUserType $ getArgType a then
    areCorrectArgTypes as
  else
    throwError (WrongTypeDefinitionException (argToPos a) (argToIdent a))

getType :: Expr -> GetterMonad EnvType 

--Vars.
getType (EVar pos id) =
  do
    env <- ask
    case gete env id of
      Just t -> return t
      Nothing -> throwError (UnexpectedToken pos id)

--Trivial.
getType (ELitFalse _) = return EnvBool
getType (ELitTrue _) = return EnvBool
getType (ELitInt _ _) = return EnvInt
getType (EString _ _) = return EnvStr

--Function application.
getType (EApp pos id args) =
  do
    env <- ask
    case gete env id of
      Just t -> checkFunction pos id t args
      Nothing -> throwError (UnexpectedToken pos id)

--Expect bool.
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

--Expect int.
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

getType (ERel pos e1 op e2) =
  do
    evType1 <- getType e1
    evType2 <- getType e2
    if isEqOperator op then
      compareType pos evType1 evType2
    else
      compareTypes pos EnvInt evType1 evType2
    return EnvBool 

--Expect String.
getType (ECon pos e1 _ e2) =
  do
    evType1 <- getType e1
    evType2 <- getType e2
    compareTypes pos EnvStr evType1 evType2
    return EnvStr 

instance Checker Program where

  checkType (PProgram pos defs) =
    do
      checkDefs 1 defs S.empty
      mapM_ checkType defs
      checkMain $ findByIdent defs

instance Checker Def where

  --Variable definition.
  checkType (GlDef pos t id e) =
    do
      env <- get
      unless (correctUserType t) $
        throwError (WrongTypeDefinitionException pos id)
      let ext = toEnvType t
      checkResult $ runComparator pos ext e env
      put (pute env id ext)

  --Function definition.
  checkType (FnDef pos rt id args block) =
    do
      unless (uniqueArgs args) $
        throwError (DuplicateFunctionArgumentsException pos)
      when (defaultFunOverride id) $
        throwError (DefaultOverrideException pos)
      areCorrectArgTypes args
      let stmts = blockToStmts block
      checkInitsRedef stmts (S.insert id (S.fromList (P.map argToIdent args)))
      env <- get
      put $ pute env id (funToEnvType rt args)
      newEnv <- get
      put $ puteArgs newEnv (argTypes args)
      beforeEnv <- get
      put $ putrt beforeEnv (Just (toEnvType rt))
      mapM_ checkType stmts
      afterEnv <- get
      checkCondition (isReturn afterEnv) (NoReturnException pos)
      put newEnv

instance Checker Block where

  checkType (BBlock _ stmts) = 
    do
      checkInitsRedef stmts S.empty
      mapM_ checkType stmts

instance Checker Stmt where

  --Empty statement.
  checkType (SEmpty _) = return ()

  --Block.
  checkType (SBStmt _ block) =
    do
      env <- get
      checkType block
      put env

  --Init and Assignment.
  checkType (SInit _ def) = checkType def

  checkType (SAss pos id e) =
    do
      env <- get
      case gete env id of
        Just et -> checkResult $ runComparator pos et e env
        Nothing -> throwError (UnexpectedToken pos id)

  --Increment and Decrement.
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

  --Returns.
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
        Just et -> checkCondition (et == EnvVoid) (BadType pos et EnvVoid)
        Nothing -> throwError (UnexpectedReturn pos)
      put $ putir env True

  --Conditionals.
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

  --While.
  checkType (SWhile pos cond block) =
    do
      env <- get
      checkResult $ runComparator pos EnvBool cond env
      checkType block

  --Expression.
  checkType (SExp pos e) =
    do
      env <- get
      checkResult $ runComparator pos EnvVoid e env

-- Runs the type checker.
runTypeCheck :: Program -> Either TypeCheckException ()
runTypeCheck program =
  runExcept $ evalStateT (checkType program) initEnv