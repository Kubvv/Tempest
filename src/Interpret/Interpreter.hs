{-# LANGUAGE FlexibleInstances #-}
module Interpret.Interpreter where

import Data.Maybe
import Syntax.AbsTempest
import Control.Monad.Except
import Control.Monad.State
import Interpret.InterpreterHelper
import Interpret.InterpreterData

instance Interpreter Program where

  interpret (PProgram pos defs) =
    do
      mapM_ interpret defs
      interpret $ fromJust $ findMainBlock defs

instance Interpreter Def where

  --Variable definition
  interpret (GlDef _ _ id e) =
    do
      mem <- get
      r <- interpret e
      put $ putS id r mem
      return RNothing

  --Function definition
  interpret (FnDef _ _ id args block) =
    do
      mem <- get
      let r = RFun args block (env mem)
      put $ putS id r mem
      return RNothing

instance Interpreter Block where

  interpret (BBlock _ stmts) =
    do
      mapM_ interpret stmts
      return RNothing

instance Interpreter Stmt where

  interpret (SEmpty _) = return RNothing

  interpret (SBStmt _ b) = interpretIfNotRet $
    do
      mem <- get
      let envi = env mem
      interpret b
      newMem <- get
      put $ putEnv envi newMem
      return RNothing

  interpret (SInit _ def) = interpretIfNotRet $
    do
      interpret def

  interpret (SAss _ id e) = interpretIfNotRet $
    do
      mem <- get
      r <- interpret e
      put $ updateS id r mem
      return RNothing

  interpret (SIncr _ id) = interpretIfNotRet $
    do
      mem <- get
      let r = fromJust $ incrResult $ getS id mem
      put $ updateS id r mem
      return RNothing

  interpret (SDecr _ id) = interpretIfNotRet $
    do
      mem <- get
      let r = fromJust $ decrResult $ getS id mem
      put $ updateS id r mem
      return RNothing

  interpret (SRet _ e) = interpretIfNotRet $
    do
      mem <- get
      r <- interpret e
      put $ putReturn r mem
      return RNothing

  interpret (SVRet _) = interpretIfNotRet $
    do
      mem <- get
      put $ putReturn RVoid mem
      return RNothing

  interpret (SCond _ cond block) = interpretIfNotRet $
    do
      mem <- get
      b <- interpret cond
      let envi = env mem
      if fromJust $ extractBool b then
        interpret block
      else
        return RNothing
      newMem <- get
      put $ putEnv envi newMem
      return RNothing

  interpret (SCondElse _ cond block1 block2) = interpretIfNotRet $
    do
      mem <- get
      b <- interpret cond
      let envi = env mem
      if fromJust $ extractBool b then
        interpret block1
      else
        interpret block2
      newMem <- get
      put $ putEnv envi newMem
      return RNothing

  interpret wh@(SWhile _ cond block) = interpretIfNotRet $
    do
    mem <- get
    b <- interpret cond
    let envi = env mem
    if fromJust $ extractBool b then
      interpret block >>
      interpret wh
    else
      return RNothing
    newMem <- get
    put $ putEnv envi newMem
    return RNothing

  interpret (SExp _ e) = interpretIfNotRet $
    do
      interpret e


instance Interpreter Expr where

  --Vars
  interpret (EVar _ id) =
    do
      gets (getS id)

  --Trivial
  interpret (ELitInt _ x) = return $ RInt x
  interpret (ELitTrue _) = return $ RBool True
  interpret (ELitFalse _) = return $ RBool False
  interpret (EString _ str) = return $ RStr str

  --Function application
  interpret (EApp _ id args) =
    if showIdent id `elem` defaults then
      do
        rs <- mapM interpret args
        interpretDefault id rs
    else
      interpretFromEnvironment id args

  --Negation and Not
  interpret (ENeg _ e) =
    do
      r <- interpret e
      let x = fromJust $ extractInt r
      return $ RInt (-x)

  interpret (ENot _ e) =
    do
      r <- interpret e
      let b = fromJust $ extractBool r
      return $ RBool (not b)

  --Arithmetic
  interpret (EMul pos e1 op e2) =
    do
      let fun = getArthOperator op
      r1 <- interpret e1
      r2 <- interpret e2
      let x1 = fromJust $ extractInt r1
      let x2 = fromJust $ extractInt r2
      when (isDiv op && x2 == 0) $
        throwError (ArithmeticException pos)
      return $ RInt (fun x1 x2)

  interpret (EAdd _ e1 op e2) =
    do
      let fun = getArthOperator op
      r1 <- interpret e1
      r2 <- interpret e2
      let x1 = fromJust $ extractInt r1
      let x2 = fromJust $ extractInt r2
      return $ RInt (fun x1 x2)

  --Comparison
  interpret (ERel _ e1 op e2) =
    do
      let fun = getCompOperator op
      r1 <- interpret e1
      r2 <- interpret e2
      let x1 = fromJust $ extractInt r1
      let x2 = fromJust $ extractInt r2
      return $ RBool (fun x1 x2)

  --Logic
  interpret (EAnd _ e1 e2) =
    do
      r1 <- interpret e1
      r2 <- interpret e2
      let x1 = fromJust $ extractBool r1
      let x2 = fromJust $ extractBool r2
      return $ RBool ((&&) x1 x2)

  interpret (EOr _ e1 e2) =
    do
      r1 <- interpret e1
      r2 <- interpret e2
      let x1 = fromJust $ extractBool r1
      let x2 = fromJust $ extractBool r2
      return $ RBool ((||) x1 x2)


interpretFromEnvironment :: Ident -> [Expr] -> InterpreterMonad
interpretFromEnvironment id args =
  do
    mem <- get
    let envi = env mem
    rs <- mapM interpret args
    let locs = map (getArgLoc envi) args
    let fun = getS id mem
    let (argsF, blockF, envF) = fromJust $ extractFun fun
    modify $ putEnv envF
    modify $ putS id fun
    putArgs rs locs argsF
    interpret blockF
    newMem <- get
    let result = returnR newMem
    modify $ putEnv envi
    modify $ putReturn RNothing
    return result

runInterpreter :: Program -> IO (Either InterpretException Result)
runInterpreter program =
  runExceptT $ evalStateT (interpret program) emptyState