import MBot

speed = 125

main = do
	d <- openMBot
	forever follow
	closeMBot d
	where
		follow = do
      sensor <- sendCommand d $ readLineFollower
      act sensor
    act LEFTB = sendCommand d $ setMotor (-speed) speed
    act RIGHTB = sendCommand d $ setMotor speed (-speed)
    act BOTHB = sendCommand d $ setMotor speed speed
    act BOTHW = sendCommand d $ setMotor 0 0
