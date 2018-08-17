-- NmrParser.hs
-- Maximiliaan Leyman

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

-- Dit gegevenstype stelt de verschillende vormen van numerieke expressie in Hasky voor
data Nmr = Lit Int
        | Var Name
        | Nmr :+: Nmr
        | Nmr :-: Nmr
        | Nmr :*: Nmr
        | Nmr :/: Nmr
        deriving (Eq, Show)

-- Parse een Hasky Nmr expressie
parseNmr :: Parser Nmr
parseNmr = parseLit `mplus` parseAdd `mplus` parseSub `mplus` parseMul `mplus` parseDiv `mplus` parseVar
    where
        parseLit = do { n <- parseInt;  -- Parse een literal
                        return (Lit n) }
        parseAdd = do { token '(';      -- Parse een optelling
                        a <- parseNmr;
                        token '+';
                        b <- parseNmr;
                        token ')';
                        return (a :+: b) }
        parseSub = do { token '(';      -- Parse een aftrekking
                        a <- parseNmr;
                        token '-';
                        b <- parseNmr;
                        token ')';
                        return (a :-: b) }
        parseMul = do { token '(';      -- Parse een vermenigvuldiging
                        a <- parseNmr;
                        token '*';
                        b <- parseNmr;
                        token ')';
                        return (a :*: b) }
        parseDiv = do { token '(';      -- Parse een deling
                        a <- parseNmr;
                        token '/';
                        b <- parseNmr;
                        token ')';
                        return (a :/: b) }
        parseVar = do { match "Var ";  -- Parse een variabele en zijn naam
                        a <- parseWord;
                        return (Var a) }
