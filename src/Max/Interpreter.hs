import CmdParser (Env, parseCmd, parsePgm, Cmd (..), Robocmd (..)   )
import Parser
import NmrParser (Nmr (..), Name)
import ConParser (Con)
import Evaluator (evalCon, evalNmr, addToEnv)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Concurrent
import MBot
import System.HIDAPI
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
    readpgm <- readFile "pgm3.txt";
    bot <- openMBot;
    evalStateT (runPgm (parse parsePgm (rTnN readpgm)) bot) [];
    closeMBot bot

    
}

-- Remove tabs and Newlines
rTnN :: String -> String
rTnN ('\t':rest) = rTnN rest
rTnN ('\r':rest) = rTnN rest
rTnN ('\n':rest) = rTnN rest
rTnN (x:rest)    = x:rTnN rest
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

type EnvIO a = StateT Env IO a

runPgm :: [Cmd] -> Device -> EnvIO ()
runPgm [] bot = return ();
runPgm (x:xs) bot = do { a <- runCmd x bot; -- Run een command en houd de IO bij in a
                         runPgm xs bot;
--                         b <- runPgm xs; -- Run de andere commands en houd de IO bij in b
--                         lift (a >> b)
                         }  -- Maak een StateT Env IO () met de twee IO's in
                            -- Het geheel resulteert in een StateT Env IO met erin de gecombineerde IO
                            -- Is lift juist?

-- De runCmd functie wordt opgeroepen per commando en geeft een StateT Env IO terug.
-- Hierbij is er een State-deel die de State - onze environment - aanpast, en een
-- IO deel, dat interfacing met de robot inhoudt.
-- ZIe verdere beschrijving in het rapport

runCmd :: Cmd -> Device -> EnvIO ()
runCmd (Wait a) bot = do{
    liftIO (print "Waiting");
    lst <- get;
    liftIO (threadDelay (100000 * evalNmr a lst))
}
runCmd Comment bot  = liftIO (print "There is a comment here")
runCmd (Call a b) bot = do {
    -- lst <- get
    -- io vs liftIO?
    liftIO (print "Calling");
    lst <- get;
    put (addToEnv a (Lit (evalNmr b lst)) lst);
    lst2 <- get;
    liftIO (print lst2);
    -- put (a,b):lst
    -- return () 
    }
-- runCmd (Check a b) = liftIO (print "Check command")
runCmd (Check a b) bot = do {
    e <- get;
    liftIO (print "Check!");
    liftIO (print "e is:");
    liftIO (print (show e));
    liftIO (print "a is:");
    liftIO (print (show a));
    if evalCon a e then runPgm b bot else return ()
}
runCmd (While a b) bot = do {
    e <- get;
    if evalCon a e then do {runPgm b bot;runCmd (While a b) bot;} else return ()
}
runCmd (Robo a) bot = runRobo a bot

runRobo :: Robocmd -> Device -> EnvIO ()
runRobo TurnLeft bot    = do{
    liftIO (print "Robot turning left");
    liftIO (sendCommand bot $ setMotor 0 150)
}
runRobo TurnRight bot   = do{
    liftIO (print "Robot turning right");
    liftIO (sendCommand bot $ setMotor 150 0)
}
runRobo Forward bot      = do{
    liftIO (print "Robot going forward");
    liftIO (sendCommand bot $ setMotor 150 150)
}
runRobo Stop bot         = do{
    liftIO (print "Robot stopping");
    liftIO (sendCommand bot $ setMotor 0 0)
}
runRobo (Lamp a b c d) bot = do{
    --liftIO (print ("Setting robot lamp: " ++ show a ++ show b ++ show c ++ show d))
    lst <- get;
    liftIO (sendCommand bot $ setRGB (evalNmr a lst) (evalNmr b lst) (evalNmr c lst) (evalNmr d lst))
}
runRobo (Light a) bot      = do {
    --liftIO (print ("Saving the default light value of 1 to " ++ a));
    --runCmd (Call a (Lit 1)) bot;
    liftIO (print "Reading the line sensor");
    lin <- liftIO (readLineFollower bot);
    runCmd (Call a (Lit (lineToInt lin))) bot
}
runRobo (Dist a) bot      = do{
    liftIO (print "Reading the ultrasonic sensor");
    val <- liftIO (readUltraSonic bot);
    runCmd (Call a (Lit (round val))) bot
}

lineToInt :: Line -> Int
lineToInt BOTHB = 0
lineToInt LEFTB = 1
lineToInt RIGHTB = 2
lineToInt BOTHW = 3

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