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

uniqueArgs :: [Arg] -> Bool
uniqueArgs as = length as == length (nubfil $ P.map argToIdent as)

defToIdent :: Def -> Ident
defToIdent (FnDef _ _ id _ _) = id
defToIdent (GlDef _ _ id _) = id

uniqueDefs :: [Def] -> Bool
uniqueDefs ds = length ds == length (nubfil $ P.map defToIdent ds)

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