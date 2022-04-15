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
      interpret (EApp pos (Ident "main") []) --TODO

instance Interpreter Def where

  --Variable definition
  interpret (GlDef _ _ id e) =
    do
      mem <- get
      v <- interpret e
      put $ putS id v mem
      return VNothing

  --Function definition
  interpret (FnDef _ _ id args block) =
    do
      mem <- get
      let v = VFun args block (env mem)
      put $ putS id v mem
      return VNothing

instance Interpreter Block where

  interpret (BBlock _ stmts) =
    do
      mapM_ interpret stmts
      return VNothing

instance Interpreter Stmt where

  interpret (SEmpty _) = return VNothing

  interpret (SBStmt _ b) = interpretIfNotRet $
    do
      mem <- get
      let envi = env mem
      interpret b
      put $ putEnv envi mem
      return VNothing

  interpret (SInit _ def) = interpretIfNotRet $
    do
      interpret def

  interpret (SAss _ id e) = interpretIfNotRet $
    do
      mem <- get
      v <- interpret e
      put $ updateS id v mem
      return VNothing

  interpret (SIncr _ id) = interpretIfNotRet $
    do
      mem <- get
      let v = fromJust $ incrValue $ getS id mem
      put $ updateS id v mem
      return VNothing

  interpret (SDecr _ id) = interpretIfNotRet $
    do
      mem <- get
      let v = fromJust $ decrValue $ getS id mem
      put $ updateS id v mem
      return VNothing

  interpret (SRet _ e) = interpretIfNotRet $
    do
      mem <- get
      v <- interpret e
      put $ putReturn v mem
      return VNothing

  interpret (SVRet _) = interpretIfNotRet $
    do
      mem <- get
      put $ putReturn VVoid mem
      return VNothing

  interpret (SCond _ cond block) = interpretIfNotRet $
    do
      mem <- get
      b <- interpret cond
      let envi = env mem
      if fromJust $ extractBool b then
        interpret block
      else
        return VNothing
      put $ putEnv envi mem
      return VNothing

  interpret (SCondElse _ cond block1 block2) = interpretIfNotRet $
    do
      mem <- get
      b <- interpret cond
      let envi = env mem
      if fromJust $ extractBool b then
        interpret block1
      else
        interpret block2
      put $ putEnv envi mem
      return VNothing

  interpret wh@(SWhile _ cond block) = interpretIfNotRet $
    do
    mem <- get
    b <- interpret cond
    let envi = env mem
    if fromJust $ extractBool b then
      interpret block >>
      interpret wh
    else
      return VNothing
    put $ putEnv envi mem
    return VNothing

  interpret (SExp _ e) = interpretIfNotRet $
    do
      interpret e


instance Interpreter Expr where

  --Vars
  interpret (EVar _ id) =
    do
      gets (getS id)

  --Trivial
  interpret (ELitInt _ x) = return $ VInt x
  interpret (ELitTrue _) = return $ VBool True
  interpret (ELitFalse _) = return $ VBool False
  interpret (EString _ str) = return $ VStr str

  --Function application
  interpret (EApp _ id args) =
    do
      mem <- get
      let envi = env mem
      vs <- mapM interpret args
      let locs = map (getArgLoc envi) args
      let fun = getS id mem
      put $ putEnv (fromJust $ getFunEnv fun) mem
      put $ putS id fun mem
      putArgs vs locs (fromJust $ getFunArgs fun)
      interpret $ fromJust $ getFunBlock fun
      newMem <- get
      let result = returnV newMem
      put $ putEnv envi newMem
      put $ putReturn VNothing newMem
      return result

  --Negation and Not
  interpret (ENeg _ e) =
    do
      v <- interpret e
      let x = fromJust $ extractInt v
      return $ VInt (-x)

  interpret (ENot _ e) =
    do
      v <- interpret e
      let b = fromJust $ extractBool v
      return $ VBool (not b)

  --Arithmetic
  interpret (EMul pos e1 op e2) =
    do
      let fun = getArthOperator op
      v1 <- interpret e1
      v2 <- interpret e1
      let x1 = fromJust $ extractInt v1
      let x2 = fromJust $ extractInt v2
      when (isDiv op && x2 == 0) $
        throwError (ArithmeticException pos)
      return $ VInt (fun x1 x2)

  interpret (EAdd _ e1 op e2) =
    do
      let fun = getArthOperator op
      v1 <- interpret e1
      v2 <- interpret e1
      let x1 = fromJust $ extractInt v1
      let x2 = fromJust $ extractInt v2
      return $ VInt (fun x1 x2)

  --Comparison
  interpret (ERel _ e1 op e2) =
    do
      let fun = getCompOperator op
      v1 <- interpret e1
      v2 <- interpret e1
      let x1 = fromJust $ extractInt v1
      let x2 = fromJust $ extractInt v2
      return $ VBool (fun x1 x2)

  --Logic
  interpret (EAnd _ e1 e2) =
    do
      v1 <- interpret e1
      v2 <- interpret e1
      let x1 = fromJust $ extractBool v1
      let x2 = fromJust $ extractBool v2
      return $ VBool ((&&) x1 x2)

  interpret (EOr _ e1 e2) =
    do
      v1 <- interpret e1
      v2 <- interpret e1
      let x1 = fromJust $ extractBool v1
      let x2 = fromJust $ extractBool v2
      return $ VBool ((||) x1 x2)

runInterpreter :: Program -> IO (Either InterpretException Value)
runInterpreter program =
  runExceptT $ evalStateT (interpret program) emptyState