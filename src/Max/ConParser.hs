module ConParser
( Con (..)
, parseCon
) where

import Parser
import MonadPlus
import NmrParser
import Debug.Trace

data Con = Boolean Bool
        | Inv Con
        | Nmr :<=: Nmr
        | Con :&: Con
        | Con :|: Con
        deriving (Eq, Show)

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
    parseCmp = do { token '(';          -- Parse een vergelijking
                    a <- parseNmr;
                    match " <= ";
                    b <- parseNmr;
                    token ')';
                    return (a :<=: b) }
    

parseBool :: Parser Con
parseBool = parseTrue `mplus` parseFalse
    where
        parseTrue   = do { match "True";
                           return (Boolean True) }
        parseFalse  = do { match "False";
                           return (Boolean False) }
    
        


