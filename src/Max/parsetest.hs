-- parsetest.hs
-- Maximiliaan Leyman

import CmdParser (Env, parseCmd, parsePgm, Cmd (..), Robocmd (..)   )
import Parser
import NmrParser (Nmr (..), Name)
import ConParser (Con)
import Evaluator (evalCon, evalNmr, addToEnv)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class


-- Dit extra bestand dient om de parsing van een Hasky-programma te testen, zonder dit daadwerkelijk op de robot uit te voeren.
-- Dit is vooral handig omdat de robot blokkeert als je (Hasky-)programma niet parset (en dus de main-methode uit Interpreter.hs geen programma heeft om uit te voeren), want de exception
--  hiervan zorgt ervoor dat de robot nooit gesloten wordt, en het dan nodig is GHCi te herstarten om verder te kunnen.

main = do {
    readpgm <- readFile "pgm4.txt";
    print (show (apply parsePgm (rTnN readpgm)));

}

-- Remove tabs and Newlines
rTnN :: String -> String
rTnN ('\t':rest) = rTnN rest
rTnN ('\r':rest) = rTnN rest
rTnN ('\n':rest) = rTnN rest
rTnN (x:rest)    = x:rTnN rest
rTnN []          = []