import MBot

main =  do
    d <- openMBot
    sendCommand d $ setRGB 1 100   0 100
    sendCommand d $ setRGB 2 100 100 0
    closeMBot d