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

-- De main-methode bevat de daadwerkelijke uitvoering van een programma. Deze leest het programma, opent de robot, parset en voert het programma uit, en sluit de robot dan weer.
-- Het programma om uit te voeren kan aangepast worden door de string bij readFile aan te passen.

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

-- EnvIO is een monad - de combinatie van de State en IO monads d.m.v. StateT.
-- De bijgehouden state is een Env, i.e. een array van ints.
type EnvIO a = StateT Env IO a

-- runPgm geeft de EnvIO terug die de uitvoering van het meegegeven programma voorstelt. Door dit dan met evalStateT te evalueren en mee te geven
--  aan de main-functie wordt het programma dan uiteindelijk uitgevoerd.
-- runPgm wordt ook gebruikt om sub-blokken uit te voeren. De resulterende EnvIO kan dan samengebind worden met andere commando's/programmas tot het uitendelijke volledige programma.
runPgm :: [Cmd] -> Device -> EnvIO ()
runPgm [] bot = return ();
runPgm (x:xs) bot = do { a <- runCmd x bot;
                         runPgm xs bot;
                         }


-- runCmd geeft de EnvIO terug die de uitvoering voorstelt van een enkel commando. De implementatie is anders voor elk commando.
-- Om het volgen van het programma te vergemakkelijken en inzicht te geven in de flow van het programma, printen meeste commandos ook een statement naar de console uit om te volgen.
runCmd :: Cmd -> Device -> EnvIO ()
runCmd (Wait a) bot = do{
    liftIO (print "Waiting");
    lst <- get;
    liftIO (threadDelay (100000 * (evalNmr a lst)))
}
runCmd Comment bot  = liftIO (print "There is a comment here")
runCmd (Call a b) bot = do {
    liftIO (print "Call statement");
    lst <- get;
    put (addToEnv a (Lit (evalNmr b lst)) lst);
    lst2 <- get;
    liftIO (print lst2);
    }
runCmd (Check a b) bot = do {
    e <- get;
    liftIO (print "Check statement");
    liftIO (print "Current env state:");
    liftIO (print e);
    liftIO (print "Condition to evaluate:");
    liftIO (print a);
    if evalCon a e then runPgm b bot else return ()
}
runCmd (While a b) bot = do {
    e <- get;
    if evalCon a e then do {runPgm b bot;runCmd (While a b) bot;} else return ()
}
runCmd (Robo a) bot = runRobo a bot

-- runRobo stelt de uitvoering van een Robocmd voor, een commando dat interreageert met de robot.
-- De motor-waarden staan op 150 omdat tijdens het testen 255 nogal snel was. Dit kan aangepast worden om de robot sneller te laten rijden/draaien.
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
    lst <- get;
    liftIO (sendCommand bot $ setRGB (evalNmr a lst) (evalNmr b lst) (evalNmr c lst) (evalNmr d lst))
}
runRobo (Light a) bot      = do {
    liftIO (print "Reading the line sensor");
    lin <- liftIO (readLineFollower bot);
    runCmd (Call a (Lit (lineToInt lin))) bot
}
runRobo (Dist a) bot      = do{
    liftIO (print "Reading the ultrasonic sensor");
    val <- liftIO (readUltraSonic bot);
    runCmd (Call a (Lit (round val))) bot
}

-- Deze hulpfunctie neemt het Line-object dat gegeven wordt door het readLineFollower-commando van de bot en zet het om naar een integer die wij kunnen bijhouden in onze Env.
lineToInt :: Line -> Int
lineToInt BOTHB = 0
lineToInt LEFTB = 1
lineToInt RIGHTB = 2
lineToInt BOTHW = 3