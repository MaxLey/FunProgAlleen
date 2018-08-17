-- ConParser.hs
-- Maximiliaan Leyman

module ConParser
( Con (..)
, parseCon
) where

import Parser
import MonadPlus
import NmrParser
import Debug.Trace

-- Dit gegevenstype stelt de verschillende vormen van logische expressies in Hasky voor
data Con = Boolean Bool
        | Inv Con
        | Con :&: Con
        | Con :|: Con
        | Nmr :<=: Nmr
        | Nmr :<: Nmr
        | Nmr :>=: Nmr
        | Nmr :>: Nmr
        | Nmr :==: Nmr
        deriving (Eq, Show)

-- Parse een Hasky Con-expressie
parseCon :: Parser Con
parseCon = parseBool `mplus` parseNot `mplus` parseAnd `mplus` parseOr `mplus` parseCmp
    where
    parseNot = do { token '(';          -- Parse een 'not' expressie
                    match "not ";
                    a <- parseCon;
                    token ')';
                    return (Inv a) }
    parseAnd = do { token '(';          -- Parse een 'and' expressie
                    a <- parseCon;
                    match " & ";
                    b <- parseCon;
                    token ')';
                    return (a :&: b) }
    parseOr  = do { token '(';          -- Parse een 'or' expressie
                    a <- parseCon;
                    match " | ";
                    b <- parseCon;
                    token ')';
                    return (a :|: b) }

parseCmp :: Parser Con
parseCmp = parseLte `mplus` parseLt `mplus` parseGte `mplus` parseGt `mplus` parseEq
    where
    parseLte =  do { token '(';          -- Parse een vergelijking
                    a <- parseNmr;
                    match " <= ";
                    b <- parseNmr;
                    token ')';
                    return (a :<=: b) }
    parseLt =   do { token '(';
                    a <- parseNmr;
                    match " < ";
                    b <- parseNmr;
                    token ')';
                    return (a :<: b) }
    parseGte =  do { token '(';
                    a <- parseNmr;
                    match " >= ";
                    b <- parseNmr;
                    token ')';
                    return (a :>=: b) }
    parseGt =   do { token '(';
                    a <- parseNmr;
                    match " > ";
                    b <- parseNmr;
                    token ')';
                    return (a :>: b) }
    parseEq =   do { token '(';
                    a <- parseNmr;
                    match " == ";
                    b <- parseNmr;
                    token ')';
                    return (a :==: b) }
    

parseBool :: Parser Con
parseBool = parseTrue `mplus` parseFalse
    where
        parseTrue   = do { match "True";
                           return (Boolean True) }
        parseFalse  = do { match "False";
                           return (Boolean False) }
    
        


