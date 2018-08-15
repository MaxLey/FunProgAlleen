module Evaluator
( evalNmr
, evalCon
, addToEnv
) where

import Parser
import MonadPlus
import NmrParser (Nmr (..), Env, Name)
import ConParser (Con (..))

-- Deze functie zoekt een nummer op in de Env
findvar :: Name -> Env -> Nmr
findvar a ((n,l):xs)    = if a == n then l else findvar a xs
findvar a []            = Lit 0

addToEnv :: Name -> Nmr -> Env -> Env
addToEnv a v ((n,l):xs)   = if a == n then (n,v):xs else (n,l):addToEnv a v xs
addToEnv a v []           = [(a,v)]

-- Deze functie evalueert een "Nmr" tot een Haskell int
evalNmr :: Nmr -> Env -> Int
evalNmr (Lit n)     e   = n
evalNmr (a :+: b)   e   = evalNmr a e + evalNmr b e
evalNmr (a :-: b)   e   = evalNmr a e - evalNmr b e
evalNmr (a :*: b)   e   = evalNmr a e * evalNmr b e
evalNmr (Var n)     e   = evalNmr (findvar n e) e

-- Deze functie evalueert een "Con" tot een Haskell Bool
evalCon :: Con -> Env -> Bool
evalCon (Boolean x) e    = x
evalCon (Inv x)     e    = not (evalCon x e)
evalCon (a :&: b)   e    = evalCon a e && evalCon b e
evalCon (a :|: b)   e    = evalCon a e || evalCon b e
evalCon (a :<=: b)  e    = evalNmr a e <= evalNmr b e
evalCon (a :>=: b)  e    = evalNmr a e >= evalNmr b e
evalCon (a :==: b)  e    = evalNmr a e == evalNmr b e