import CmdParser (Env, parseCmd, parsePgm, Cmd (..), Robocmd (..)   )
import Parser
import NmrParser (Nmr (..), Name)
import ConParser (Con)
import Evaluator (evalCon, evalNmr, addToEnv)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
-- import Data.Text.IO as T
-- import Mbot

-- Deze file is niet compleet omdat ik er niet in geslaagd ben de
-- Monad transformer juist te gebruiken. Meer informatie over de
-- implementatie van de commandos staat in het rapport.

-- Normaal zou hier in de main-methode de Mbot geopend worden, dan de
-- file met het programma ingelezen worden. Hierna wordt de code door de parser
-- omgezet naar een list van commando's.
-- Uiteindelijk kan dan met runStateT en te beginnen met een lege omgeving het programma
-- uitgevoerd worden, en uiteindelijk de robot gesloten worden.

-- Voor een demonstratie van de werkende delen heb ik de echte main-methode
-- Vervangen door een demonstratie van de parser.

main = do {
    --evalStateT (runPgm $ parse parsePgm ("{Call iets 5;Call nogiets 3;Forward;Wait;if(((Var iets) <= 3)){Wait;Lamp 1 255 0 0;Light lightValue;Dist distanceValue;}}")) [("test",Lit 2)]
    --{if((Var iets <= 3)){Wait;}Call iets 5;if((Var iets <= 3)){Turn_left;}}
    readpgm <- readFile "pgm2.txt";
    evalStateT (runPgm $ parse parsePgm (rTnN readpgm)) [];
}

-- Remove tabs and Newlines
rTnN :: [Char] -> [Char]
rTnN ('\t':rest) = rTnN rest
rTnN ('\r':rest) = rTnN rest
rTnN ('\n':rest) = rTnN rest
rTnN (x:rest)    = x:(rTnN rest)
rTnN []          = []


--main = do {
--    d <- openMbot;
--    pgm <- readFile "programma.txt";
--    -- Verwijder newlines en evt tabs;
--    -- runStateT (runPgm (parse parsePgm pgm)) [];
--    closeMbot d;
--}

-- Elke lijn doet de IO, en storet de variabele in wat er links staat
-- Dus de IO die we uit runPgm halen moet de hele IO-interactie van het
-- programma zijn.
-- De state wordt maar bijgehouden zolang runPgm leeft

type MyMonad a = StateT Env IO a

runPgm :: [Cmd] -> MyMonad ()
runPgm [] = return ();
runPgm (x:xs) = do { a <- runCmd x; -- Run een command en houd de IO bij in a
                     runPgm xs;
--                     b <- runPgm xs; -- Run de andere commands en houd de IO bij in b
--                     lift (a >> b)
                     } -- Maak een StateT Env IO () met de twee IO's in
                        -- Het geheel resulteert in een StateT Env IO met erin de gecombineerde IO
                        -- Is lift juist?

-- De runCmd functie wordt opgeroepen per commando en geeft een StateT Env IO terug.
-- Hierbij is er een State-deel die de State - onze environment - aanpast, en een
-- IO deel, dat interfacing met de robot inhoudt.
-- ZIe verdere beschrijving in het rapport

runCmd :: Cmd -> MyMonad ()
runCmd Wait = liftIO (print "Waiting")
runCmd Comment = liftIO (print "There is a comment here")
runCmd (Call a b) = do {
    -- lst <- get
    -- io vs liftIO?
    liftIO (print ("Calling"));
    lst <- get;
    put (addToEnv a (Lit (evalNmr b lst)) lst);
    lst2 <- get;
    liftIO (print lst2);
    -- put (a,b):lst
    -- return () 
    }
-- runCmd (Check a b) = liftIO (print "Check command")
runCmd (Check a b) = do {
    e <- get;
    liftIO (print ("Check!"));
    liftIO (print ("e is:"));
    liftIO (print (show e));
    liftIO (print ("a is:"));
    liftIO (print (show a));
    if (evalCon a e) then (runPgm b) else return ()
}
runCmd (While a b) = do {
    e <- get;
    if (evalCon a e) then do {runPgm b;runCmd (While a b);} else return ()
}
runCmd (Robo a) = runRobo a

runRobo :: Robocmd -> MyMonad ()
runRobo Turn_Left    = liftIO (print ("Robot turning left"))
runRobo Turn_Right   = liftIO (print ("Robot turning right"))
runRobo Forward      = liftIO (print ("Robot going forward"))
runRobo Stop         = liftIO (print ("Robot stopping"))
runRobo (Lamp a b c d) = liftIO (print ("Setting robot lamp: " ++ (show a) ++ (show b) ++ (show c) ++ (show d)))
runRobo (Light a)      = do {
    liftIO (print ("Saving the default light value of 1 to " ++ a));
    runCmd (Call a (Lit 1));
}
runRobo (Dist a)       = do{
    liftIO (print ("Saving the default distance value of 10 to " ++ a));
    runCmd (Call a (Lit 10));
}

-- data StateIO s a = StateIO {
--     runStateIO :: s -> (IO a, s)
-- }

-- instance Functor (StateIO s) where
--     fmap f ex = StateIO $ \s -> (fmap f inside, s)
--         where
--         inside s = 


-- liftState :: State s a -> StateT Env IO ()
-- liftState x = StateT $ \s -> IO ()

-- data StateT s m a = StateT {
--     runStateT :: s -> m (a, s)   
-- }