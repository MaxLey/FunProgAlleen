module NmrParser
( Nmr (..)
, Name
, Env
, parseNmr
) where

import Parser
import MonadPlus

type Name = String
type Env = [(Name, Nmr)]

data Nmr = Lit Int
        | Var Name
        | Nmr :+: Nmr
        | Nmr :*: Nmr
        deriving (Eq, Show)


parseNmr :: Parser Nmr
parseNmr = parseLit `mplus` parseAdd `mplus` parseMul `mplus` parseVar
    where
    parseLit = do { n <- parseInt;  -- Parse een literal
                    return (Lit n) }
    parseAdd = do { token '(';      -- Parse een optelling
                    a <- parseNmr;
                    token '+';
                    b <- parseNmr;
                    token ')';
                    return (a :+: b) }
    parseMul = do { token '(';      -- Parse een vermenigvuldiging
                    a <- parseNmr;
                    token '*';
                    b <- parseNmr;
                    token ')';
                    return (a :*: b) }
    parseVar = do { match ("Var ");
                    a <- parseWord;
                    --token ')';
                    return (Var a) }
