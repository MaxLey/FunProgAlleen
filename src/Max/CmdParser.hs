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

data Cmd    = Wait Nmr
            | Comment
            | Call Name Nmr
            | Check Con [Cmd] -- Cmd (voor else)
            | While Con [Cmd]
            | Robo Robocmd
            deriving (Eq, Show)

-- Dit zijn de commandos die daadwerkelijk interreageren met de robot.
data Robocmd    = Turn_Left
                | Turn_Right
                | Forward
                | Stop
                | Lamp Nmr Nmr Nmr Nmr
                | Light Name
                | Dist Name
                deriving (Eq, Show)

parseCmd :: Parser Cmd
parseCmd = parseWait `mplus` parseCall
                     `mplus` parseCheck 
                     `mplus` parseWhile 
                     `mplus` parseRobo
                     `mplus` parseComment
    where
    parseWait = do { match ("Wait ");           -- Parse een 'wait' commando
                     a <- parseNmr;
                     token ';';
                     return (Wait a) }
    parseCall = do { match ("Call ");           -- Parse een 'call' commando
                     a <- parseWord;
                     token ' ';
                     b <- parseNmr;
                     token ';';
                     return (Call a b) }
    parseCheck = do { match ("if(");            -- Parse een 'if' commando
                      a <- parseCon;    -- De benaming is hier 'check' om conflict
                      token ')';        -- met Haskell te vermijden
                      b <- parsePgm;
                      return (Check a b) }
    parseWhile = do { match ("while(");         -- Parse een 'while' commando
                    a <- parseCon;
                    token ')';
                    b <- parsePgm;
                    return (While a b) }
    parseComment = do { token '#';                 -- Parse een comment line
                        plus (spot (\s -> s /= '#'));
                        token '#';
                        return (Comment) }

-- Deze parser parset de verschillende robot-commandos.
parseRobo :: Parser Cmd
parseRobo = parseLeft `mplus` parseRight
                      `mplus` parseFwd
                      `mplus` parseStop
                      `mplus` parseLamp
                      `mplus` parseLight
                      `mplus` parseDist
    where
    parseLeft   = do { match ("Turn_left;");
                        return (Robo Turn_Left) }
    parseRight  = do { match ("Turn_right;");
                        return (Robo Turn_Right) }
    parseFwd    = do { match ("Forward;");
                        return (Robo Forward) }
    parseStop   = do { match ("Stop;");
                        return (Robo Stop) }
    parseLamp   = do { match ("Lamp ");
                        a <- parseNmr;
                        token ' ';
                        b <- parseNmr;
                        token ' ';
                        c <- parseNmr;
                        token ' ';
                        d <- parseNmr;
                        token ';';
                        return (Robo (Lamp a b c d)) }
    parseLight  = do { match ("Light ");
                        a <- parseWord;
                        token ';';
                        return (Robo (Light a)) }
    parseDist   = do { match ("Dist ");
                        a <- parseWord;
                        token ';';
                        return (Robo (Dist a)) }

parsePgm :: Parser [Cmd]
parsePgm = do{  token '{';
                res <- plus parseCmd;
                token '}';
                return res }