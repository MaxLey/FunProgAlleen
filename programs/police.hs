module BlinkingLights (main) where

import MBot
import Control.Concurrent.Thread.Delay

main = do
	d <- openMBot
	forever blink
	closeMBot d
	where
		blink = do
			sendCommand d $ setRGB 0 255 255 255
			sendCommand d $ setRGB 1 0 0 0
			delay 5
			sendCommand d $ setRGB 0 0 0 0
			sendCommand d $ setRGB 1 255 255 255
			delay 5
