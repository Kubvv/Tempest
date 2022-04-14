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
      s <- get 
      v <- interpret e
      put $ putS id v s
      return VNothing

  --Function definition
  interpret (FnDef _ _ id args block) =
    do
      s <- get
      let v = VFun args block (env s)
      put $ putS id v s
      return VNothing 

instance Interpreter Block where

  interpret (BBlock _ stmts) =
    do
      mapM_ interpret stmts
      return VNothing

instance Interpreter Stmt where

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
  interpret (EApp _ id args) = return $ VInt 0

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