module Main(main) where

import Simulator
import Control.Concurrent
import Control.Monad
import System.Random

speed = 0.5

main = do
  -- print "Opening Simulator thread"
  d <- openMBot
  -- print "Starting program thread"
  -- forever (police d)
  forever $ police d
  closeMBot d
  -- print "Closed Simulator thread"

police d = do
  sendCommand d $ setRGB 1 255 0 0
  sendCommand d $ setRGB 2 255 0 0
  threadDelay 500000
  sendCommand d $ setRGB 1 0 0 255
  sendCommand d $ setRGB 2 0 0 255
  threadDelay 500000

testCommands d = do
  dist <- readUltraSonic d
  print dist
  line <- readLineFollower d
  print line
  threadDelay 500000
  police d

avoidRandom d = do
      sensor <- readUltraSonic d
      act sensor
        where act sensor | sensor >= 3 = sendCommand d $ setMotor speed speed
                         | otherwise = do
                              number <- randomRIO (0,1) :: IO Int
                              handle number
                              threadDelay 800000
                                where handle 1 = sendCommand d $ setMotor speed (-speed)
                                      handle _ = sendCommand d $ setMotor (-speed) speed

avoidAlwaysLeft d = do
      sensor <- readUltraSonic d
      act sensor
        where act sensor | sensor >= 3 = sendCommand d $ setMotor speed speed
                         | otherwise = do
                              sendCommand d $ setMotor speed (-speed)
                              threadDelay 700000
