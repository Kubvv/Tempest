module Typecheck.TypeCheckerHelper where

import Prelude as P
import Data.Map as M

import Typecheck.TypeCheckerData
import Syntax.AbsTempest

nubfil :: Eq a => [a] -> [a]
nubfil [] = []
nubfil (x:xs) = x : nubfil (P.filter (/=x) xs)

argToIdent :: Arg -> Ident
argToIdent (VArg _ _ id) = id
argToIdent (RArg _ _ id) = id

argToType :: Arg -> EnvType
argToType (VArg _ t _) = toEnvType t
argToType (RArg _ t _) = toEnvType t

exprToPos :: Expr -> BNFC'Position
exprToPos (EVar pos _) = pos
exprToPos (ELitInt pos _) = pos
exprToPos (ELitTrue pos) = pos
exprToPos (ELitFalse pos) = pos
exprToPos (EString pos _) = pos
exprToPos (EApp pos _ _) = pos
exprToPos (ENeg pos _) = pos
exprToPos (ENot pos _) = pos
exprToPos (EMul pos _ _ _) = pos
exprToPos (EAdd pos _ _ _) = pos
exprToPos (ERel pos _ _ _) = pos
exprToPos (EAnd pos _ _)  = pos
exprToPos (EOr pos _ _) = pos

uniqueArgs :: [Arg] -> Bool
uniqueArgs as = length as == length (nubfil $ P.map argToIdent as)

defToIdent :: Def -> Ident
defToIdent (FnDef _ _ id _ _) = id
defToIdent (GlDef _ _ id _) = id

defToPos :: Def -> BNFC'Position
defToPos (FnDef pos _ _ _ _) = pos
defToPos (GlDef pos _ _ _) = pos

defaultFunOverride :: [Def] -> Bool
defaultFunOverride = P.foldr
  (\ d -> (||) (show (defToIdent d) `elem` P.map (show . fst) defaults)) False

argTypes :: [Arg] -> [(Ident, EnvType)]
argTypes [] = []
argTypes (x:xs) = (id, t) : argTypes xs
  where
    id = argToIdent x
    t = argToType x

findByIdent :: [Def] -> Maybe Def
findByIdent [] = Nothing
findByIdent (fn@(FnDef _ _ (Ident id) _ _):ds) =
  if id == "main" then
    Just fn
  else
    findByIdent ds
findByIdent (d:ds) = findByIdent ds