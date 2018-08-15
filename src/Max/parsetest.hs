import CmdParser (Env, parseCmd, parsePgm, Cmd (..), Robocmd (..)   )
import Parser
import NmrParser (Nmr (..), Name)
import ConParser (Con)
import Evaluator (evalCon, evalNmr, addToEnv)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class

main = do {
    --evalStateT (runPgm $ parse parsePgm ("{Call iets 5;Call nogiets 3;Forward;Wait;if(((Var iets) <= 3)){Wait;Lamp 1 255 0 0;Light lightValue;Dist distanceValue;}}")) [("test",Lit 2)]
    --{if((Var iets <= 3)){Wait;}Call iets 5;if((Var iets <= 3)){Turn_left;}}
    readpgm <- readFile "pgm3.txt";
    print (show (apply   parsePgm (rTnN readpgm)));

}

-- Remove tabs and Newlines
rTnN :: String -> String
rTnN ('\t':rest) = rTnN rest
rTnN ('\r':rest) = rTnN rest
rTnN ('\n':rest) = rTnN rest
rTnN (x:rest)    = x:rTnN rest
rTnN []          = []