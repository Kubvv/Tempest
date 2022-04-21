module Typecheck.TypeCheckerHelper where

import Prelude as P
import Data.Map as M

import Typecheck.TypeCheckerData
import Syntax.AbsTempest

-- Handy getters of some data types.
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
exprToPos (ECon pos _ _ _) = pos

defToIdent :: Def -> Ident
defToIdent (FnDef _ _ id _ _) = id
defToIdent (GlDef _ _ id _) = id

defToPos :: Def -> BNFC'Position
defToPos (FnDef pos _ _ _ _) = pos
defToPos (GlDef pos _ _ _) = pos

-- Checks if all arguments have unique names.
uniqueArgs :: [Arg] -> Bool
uniqueArgs as = length as == length (nubfil $ P.map argToIdent as)

nubfil :: Eq a => [a] -> [a]
nubfil [] = []
nubfil (x:xs) = x : nubfil (P.filter (/=x) xs)

-- Checks if some definition overrides some default function.
defaultFunOverride :: [Def] -> Bool
defaultFunOverride = P.foldr
  (\ d -> (||) (show (defToIdent d) `elem` P.map (show . fst) defaults)) False

-- Converts args to arrays of identifiers and arg EnvTypes.
argTypes :: [Arg] -> [(Ident, EnvType)]
argTypes [] = []
argTypes (x:xs) = (id, t) : argTypes xs
  where
    id = argToIdent x
    t = argToType x

-- Looks for main identifier among definitions.
findByIdent :: [Def] -> Maybe Def
findByIdent [] = Nothing
findByIdent (fn@(FnDef _ _ (Ident id) _ _):ds) =
  if id == "main" then
    Just fn
  else
    findByIdent ds
findByIdent (d:ds) = findByIdent ds