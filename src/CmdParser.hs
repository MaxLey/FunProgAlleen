-- CmdParser.hs
-- Maximiliaan Leyman

module CmdParser
( Cmd (..)
, Robocmd (..)
, Env
, parseCmd
, parsePgm
) where

import Parser
import MonadPlus
import ConParser
import NmrParser


-- Dit gegevenstype stelt de verschillende commandos in Hasky voor
data Cmd    = Wait Nmr          -- Wacht Nmr seconden
            | Comment           -- Een comment - doet niks
            | Call Name Nmr     -- Houd het meegegeven Nmr bij als variabele met als naam Name
            | Check Con [Cmd]   -- Voert het [Cmd] blok uit als Con 'True' evalueert
            | While Con [Cmd]   -- Voert het [Cmd] blok uit zolang Con 'True' evalueert
            | Robo Robocmd      -- Voert het robot-commando Robocmd uit
            deriving (Eq, Show)

-- Dit zijn de commandos die daadwerkelijk interreageren met de robot.
data Robocmd    = TurnLeft Nmr
                | TurnRight Nmr
                | Forward Nmr
                | Stop
                | Lamp Nmr Nmr Nmr Nmr
                | Light Name
                | Dist Name
                deriving (Eq, Show)

-- Parse een enkel Hasky-commando
parseCmd :: Parser Cmd
parseCmd = parseWait `mplus` parseCall
                     `mplus` parseCheck 
                     `mplus` parseWhile 
                     `mplus` parseRobo
                     `mplus` parseComment
    where
    parseWait = do { match "wait ";           -- Parse een 'wait' commando
                     a <- parseNmr;
                     token ';';
                     return (Wait a) }
    parseCall = do { match "call ";           -- Parse een 'call' commando
                     a <- parseWord;
                     token ' ';
                     b <- parseNmr;
                     token ';';
                     return (Call a b) }
    parseCheck = do { match "if(";            -- Parse een 'if' commando
                      a <- parseCon;    -- De benaming is hier 'check' om verwarring te vermijden, aangezien zowel Haskell als mijn programmeertaal al 'if' in de code hebben.
                      token ')';
                      b <- parsePgm;
                      return (Check a b) }
    parseWhile = do { match "while(";         -- Parse een 'while' commando
                    a <- parseCon;      -- Hlint meldt dat duplication hier vermeden kan worden met Check - dit is waar, maar zou de structuur een beetje verbrodden en het
                    token ')';          -- ingewikkelder maken dan om dit gewoon te laten staan.
                    b <- parsePgm;
                    return (While a b) }
    parseComment = do { token '#';                 -- Parse een comment line
                        plus (spot (/= '#'));
                        token '#';
                        return Comment }

-- Parse een commando dat met de robot interreageert.
parseRobo :: Parser Cmd
parseRobo = parseLeft `mplus` parseRight
                      `mplus` parseFwd
                      `mplus` parseStop
                      `mplus` parseLamp
                      `mplus` parseLight
                      `mplus` parseDist
                      `mplus` parseLeftAmount
                      `mplus` parseRightAmount
                      `mplus` parseFwdAmount
    where
    parseLeft   = do { match "turn_left;";
                        return (Robo (TurnLeft (Lit 150))) }
    parseRight  = do { match "turn_right;";
                        return (Robo (TurnRight (Lit 150))) }
    parseFwd    = do { match "forward;";
                        return (Robo (Forward (Lit 150))) }
    parseStop   = do { match "stop;";
                        return (Robo Stop) }
    parseLamp   = do { match "lamp ";
                        a <- parseNmr;
                        token ' ';
                        b <- parseNmr;
                        token ' ';
                        c <- parseNmr;
                        token ' ';
                        d <- parseNmr;
                        token ';';
                        return (Robo (Lamp a b c d)) }
    parseLight  = do { match "light ";
                        a <- parseWord;
                        token ';';
                        return (Robo (Light a)) }
    parseDist   = do { match "dist ";
                        a <- parseWord;
                        token ';';
                        return (Robo (Dist a)) }
    parseLeftAmount = do { match "turn_left ";
                            a <- parseNmr;
                            token ';';
                            return (Robo (TurnLeft a)) }
    parseRightAmount = do { match "turn_right ";
                            a <- parseNmr;
                            token ';';
                            return (Robo (TurnRight a)) }
    parseFwdAmount  = do { match "forward ";
                            a <- parseNmr;
                            token ';';
                            return (Robo (Forward a)) }

-- Parse een Hasky programma-blok, met accolades als delimiters
parsePgm :: Parser [Cmd]
parsePgm = do{  token '{';
                res <- plus parseCmd;
                token '}';
                return res }