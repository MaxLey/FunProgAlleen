import MBot
import System.Random
import Control.Concurrent.Thread.Delay

speed = 125

main = do
	d <- openMBot
	forever avoid
	closeMBot d
	where
		avoid = do
      sensor <- sendCommand d $ readUltraSonic
      act sensor
    act sensor | sensor > 10 = sendCommand d $ setMotor speed speed
							 | otherwise = do
								 		r1 <- getStdGen
   					 				let (x, r2) = randomR (0,1) r1
										handle x
										delay 5
										where
												handle x | x == 1 = sendCommand d $ setMotor speed (-speed)
										         		 | otherwise = sendCommand d $ setMotor (-speed) speed
