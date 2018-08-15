module Simulator (openMBot,
                  closeMBot,
                  readUltraSonic,
                  readLineFollower,
                  sendCommand,
                  setMotor,
                  setRGB
                 ) where

import Data.List (delete, sort, maximumBy)
import Data.Ord (comparing)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Graphics.Gloss.Data.Bitmap (loadBMP)
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Geometry.Angle
import System.Environment (getArgs)
import Control.Concurrent
import Control.Monad (join)
import Control.Arrow ((***))
import Data.Maybe (catMaybes, mapMaybe, isJust)

-- CONSTANTS --
initLED = (255, 255, 255)
initRobot = Robot (0,0) 0 0 radiusWheel distBetweenWheels 0 maxDistSensor BOTHW (initLED, initLED)
-- In the beginning, the world is empty, except for a robot at (0, 0).
emptyWorld = World initRobot [] [] 0 (0,0)
initWindow = G.InWindow "MBot" (1000,1000) (100,100)

-- Radius of wheel
radiusWheel = 20
-- Distance between wheels
distBetweenWheels = 20
-- Distance between LineFollowing sensors
distLineSensors = 1
-- Maximum detection distance for ultra sonic sensor
maxDistSensor = 3
-- Thickness of lines
lineThickness = 1.5
defaultSpeed = 0.05

-- Pixel size of a single wall
cell = 32

-- TYPES --
-- R G B values 0 - 255; (0, 0, 0) is off
type LED = (Int, Int, Int)
type Seg = (G.Point, G.Point)

-- DATA --
data Robot   = Robot { rCoord :: G.Point,
                       rVl :: Float, -- Speed of left wheel
                       rVr :: Float, -- Speed of right wheel
                       rWr :: Float, -- Radius of wheel
                       rWa :: Float, -- Distance between wheels
                       rTheta :: Float, -- Direction in radials
                       rDistance :: Float, -- Distance for ultra sonic sensor
                       rLine :: Line, -- Line for line follower sensor
                       rLEDs :: (LED, LED) -- 0 for LED 1, 1 for LED 2
                     } deriving (Eq, Ord, Show)

-- A world contains a robot, walls and lines
-- As extra it stores the viewport size of the source file and mouse position
data World = World { wRobot     :: Robot
                   , wWalls   :: [G.Point]
                   , wLines  :: [Seg]
                   , wBorder :: G.Point -- Max (x, y) in text representation
                   , wMousePos :: G.Point
                   } deriving (Eq, Ord, Show)

-- The command constructors are pattern matched by executeCommand
data Command = SetMotor Float Float |      -- vl vr
               SetRGB Int Int Int Int      -- index r g b

-- The simulator needs 3 channels for communication via the exported interface
-- One MVar for recieving commands, two for transmitting the sensor data
data Device = Device { dCommand :: MVar Command,
                       dDistance :: MVar Float,
                       dLine :: MVar Line }

data Line = LEFTB | RIGHTB | BOTHB | BOTHW deriving (Eq, Ord, Show)

lineFromBools (True , False) = LEFTB
lineFromBools (False, True ) = RIGHTB
lineFromBools (True , True ) = BOTHB
lineFromBools (False, False) = BOTHW

-- EXPORTED MBOT FUNCTIONS --
openMBot :: IO Device
openMBot = do world     <- readWorld "/Users/berombau/FunProg/src/simulator_grid.txt"
              [rp,wp]   <- mapM loadBMP [ "/Users/berombau/FunProg/src/robotOFF.bmp"
                                        , "/Users/berombau/FunProg/src/wall.bmp"]
              c         <- newEmptyMVar
              d         <- newEmptyMVar
              l         <- newEmptyMVar
              -- print "Forking Gloss"
              forkIO $ G.playIO initWindow -- display
                 G.white                   -- background
                 30                        -- fps
                 world                     -- initial world
                 (render rp wp)            -- render world
                 handleEvent               -- handle input
                 (simulateWorld (Device c d l))
              -- print "Returning Device from Simulator"
              return (Device c d l)

closeMBot :: Device -> IO ()
closeMBot d = return ()

sendCommand :: Device -> Command -> IO ()
sendCommand Device { dCommand = mvar } = putMVar mvar

readUltraSonic :: Device -> IO Float
readUltraSonic Device { dDistance = mvar } = takeMVar mvar

readLineFollower :: Device -> IO Line
readLineFollower Device { dLine = mvar } = takeMVar mvar

setMotor :: Float -> Float -> Command
setMotor = SetMotor

setRGB :: Int -> Int -> Int -> Int -> Command
setRGB = SetRGB


-- GEOMETRY FUNCTIONS --

-- Returns (dX, dY) for dt and Robot
nextPosition :: Float -> Robot -> G.Point
nextPosition dt Robot { rCoord = (x, y),
                        rVl = vl,
                        rVr = vr,
                        rWr = wr,
                        rTheta = theta } =
    (x + (dt * (wr / 2) * (vl + vr) * cos theta),
     y + (dt * (wr / 2) * (vl + vr) * sin theta))

-- Returns dTheta for dt and Robot
nextDirection :: Float -> Robot -> Float
nextDirection dt Robot { rCoord = (x, y),
                         rVl = vl,
                         rVr = vr,
                         rWr = wr,
                         rWa = wa,
                         rTheta = theta } =
    normalizeAngle $ theta + (dt * (wr / wa) * (vr - vl))

-- Returns (left, right) points of sensors, offset from Robot point by distLineSensors
getLineSensorPoints :: Float -> G.Point -> Float -> Seg
getLineSensorPoints size p theta =
    mapTuple (+ p) (mulSV (distLineSensors * size / 2) (cos (pi - theta), sin theta),
                    mulSV (distLineSensors * size / 2) (cos theta       , sin (- theta)))

-- Returns the segment representing the line of sight of the distance sensor
getSightLine :: Float -> G.Point -> Float -> Seg
getSightLine size p theta = (p + mulSV (maxDistSensor * size) (cos theta, sin theta), p)

getSegments :: Float -> G.Point -> [Seg]
getSegments s (x, y) = [((x + s, y + s), (x + s, y - s)),
                      ((x - s, y - s), (x + s, y - s)),
                      ((x - s, y - s), (x - s, y + s)),
                      ((x + s, y + s), (x - s, y + s))
                     ]
-- s is (cell / 2) when calculated on pixel points, 0.5 when on grid points

-- Returns [ 4 points ] of the rectangle for a segment and const lineThickness
getRectFromSeg :: Seg -> Path
getRectFromSeg (p1, p2) = [ transform lineThickness p1,
                            transform (-lineThickness) p1,
                            transform lineThickness p2,
                            transform (-lineThickness) p2 ]
  where transform dy p = rotateV (argV p) $ offset dy $ rotateV (-(argV p)) p
        offset y' (x, y) = (x, y + y')

-- Returns distance between two points
distance :: G.Point -> G.Point -> Float
distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2


-- UTILITY FUNCTIONS --
mapTuple = join (***)


-- GLOSS FUNCTIONS --

-- Given a list of lines, create a world. In this string:
-- - > is the position of the robot.
-- - X is the position of a wall.
--
-- For example, in:
--
--     x
--   --------------------->
-- y | +-------------------+
--   | |                   |
--   | |  X  >             |
--   | |  X                |
--   | |  XXXXXXXXXX       |
--   | |           X       |
--   | |           X       |
--   v +-------------------+
-- (0,0) (10,10)
-- (10,10) (20,10)

-- we have a robot on (6, 2), walls on (3, 2) and (3, 3) and so on.
-- two line segment at ((0,0), (10,10)) and ((10,10), (20,10))
makeWorld :: [String] -> World
makeWorld ls = addLines $ addBorder $ foldr (uncurry addPiece) emptyWorld (withCoords grid)
  where withCoords grid = [(chr, (c, r))
                            | (r, row) <- zip [0..] (reverse grid)
                            , (c, chr) <- zip [0..] row
                            ]
        addPiece '^' coord world = setRobot (pi/2)     coord world
        addPiece '>' coord world = setRobot 0          coord world
        addPiece 'v' coord world = setRobot (3/2 * pi) coord world
        addPiece '<' coord world = setRobot pi         coord world
        addPiece 'X' coord world = world { wWalls = coord:wWalls world  }
        addPiece _ _ world = world

        setRobot theta coord world = world {
                 wRobot = (wRobot world) { rTheta = theta, rCoord = coord }
                 }

        addBorder world = world {
                 wBorder = (maxX, maxY)
                 }
        (maxX, maxY) = snd $ maximumBy (comparing (\(_, (x, y)) -> x + y)) (withCoords grid)
        (grid, lineList) = span (\l -> head l `elem` "+|") ls
        addLines world = world { wLines = map (correctLine.getLine.take 2.words) lineList }
        getLine :: [String] -> Seg
        getLine [ _:p1, _:p2 ] = ((read (getFirst $ init p1), read (getSecond $ init p1))
                                , (read (getFirst $ init p2), read (getSecond $ init p2)))
        getFirst :: String -> String
        getFirst = takeWhile (/= ',')
        getSecond :: String -> String
        getSecond = tail . dropWhile (/= ',')

        correctLine :: Seg -> Seg
        correctLine = mapTuple correctPoint

        correctPoint p = centerPoint $ rotateV ((-2)*argV p) p
        centerPoint (x, y) = (x, y+maxY)

-- Given a filename, read and return a world.
readWorld :: String -> IO World
readWorld f = makeWorld . lines <$> readFile f

render :: G.Picture -> G.Picture -> World -> IO G.Picture
render rp wp gridworld = return $ G.pictures $
               map renderLines ls
            ++ map (renderPicAt wp) w
            ++ map renderLines (concatMap (getSegments (cell / 2)) w)
            ++ [renderPicAt (transformRobot $ G.pictures [drawLEDs rp r]) (rCoord r)]
            ++ [G.color (makeColorI 0 255 0 255 ) $ G.Line [sp1, sp2]]
            ++ map renderLines [getSightLine cell (rCoord r) (rTheta r)]
            ++ [ renderDebugText (-500, -200) $ G.Text $ show $ wBorder world,
                 renderDebugText (-500, -220) $ G.Text $ show $ wMousePos world,
                 renderDebugText (-500, -240) $ G.Text $ show $ wRobot gridworld,
                 renderDebugText (-500, -260) $ G.Text $ show $ wLines gridworld,
                 renderDebugText (-500, -280) $ G.Text $ show $ mapTuple (mapTuple (/ cell)) (sp1, sp2)
               ]

   where r = wRobot world
         w = wWalls world
         ls = wLines world
         world = toPixelCoord gridworld
         toPixelCoord:: World -> World
         toPixelCoord w = w { wRobot = r, wWalls = ws, wLines = ls }
           where r = (wRobot w) { rCoord = correctPoint (rCoord (wRobot w)) }
                 ws = map correctPoint (wWalls w)
                 ls = map correctLine (wLines w)
                 correctLine = mapTuple correctPoint
                 correctPoint :: G.Point -> G.Point
                 allCoord = wWalls w ++ [rCoord (wRobot w)] ++ concatMap (\(p1, p2) -> [p1, p2]) (wLines w)
                 size which = maximum $ map which allCoord
                 toPix which = (+ (cell / 2 - cell * which (wBorder w) / 2))
                             . (* cell)
                 correctPoint (x, y) = (toPix fst x - maxX / 2, toPix snd y - maxY / 2)
                 (maxX, maxY) = wBorder w
         renderDebugText (x, y) p  = renderPicAt (G.scale 0.1 0.1 p) (x, y)
         (sp1, sp2) = getLineSensorPoints cell (rCoord r) (-(rTheta r))
         renderPicAt picture (x, y) = G.translate x y picture
         -- G.rotate does clockwise rotation, so a negative angle will be counterclockwise
         transformRobot = G.rotate $ radToDeg $ (- rTheta r) + pi / 2
         drawLEDs rp Robot { rLEDs = (l1, l2) } = G.pictures [rp, drawLED l1 (-15, 15), drawLED l2 (15, 15)]
         drawLED (r, g, b) (x, y) = G.Color (makeColorI r g b 255) $ G.translate x y $ G.rectangleSolid 10 5
         renderLines :: Seg -> G.Picture
         renderLines (p1, p2) = G.Line [p1, p2]

handleEvent :: G.Event -> World -> IO World
handleEvent (G.EventKey (G.MouseButton G.LeftButton) G.Down _ (xPos, yPos)) = handle
  where handle :: World -> IO World
        handle wld = return wld { wMousePos = (xPos, yPos) }
handleEvent (G.EventKey (G.SpecialKey key) G.Up _ _) = handle key
  where handle :: G.SpecialKey -> World -> IO World
        -- Move Robot
        handle G.KeyDown   wld = setWheelSpeed (-defaultSpeed) (-defaultSpeed) wld
        handle G.KeyUp     wld = setWheelSpeed defaultSpeed defaultSpeed wld
        handle G.KeyLeft   wld = setWheelSpeed (-1) 1 wld
        handle G.KeyRight  wld = setWheelSpeed 1 (-1) wld
        -- Stop robot
        handle G.KeyDelete wld = setWheelSpeed    0 0 wld
        -- Reset position
        handle G.KeySpace  wld = changeLocation (0, 0) wld
        handle _           wld = return wld

        changeLocation coord world@World { wRobot = robot } = return world { wRobot = robot { rCoord = coord } }
        setWheelSpeed l r world = return world { wRobot = (wRobot world) { rVl = l, rVr = r }}
handleEvent _ = return

simulateWorld :: Device -> Float -> World -> IO World
simulateWorld dev timeStep world = applyDevToWorld dev $ adjustDistance $ adjustRLine world { wRobot = moveRobot $ wRobot world }
        where r = wRobot world
              moveRobot :: Robot -> Robot
              moveRobot r = r { rCoord = nextPosition timeStep r,
                                rTheta = nextDirection timeStep r
                              }
              -- Updates the current distance ultra sonic sensor value
              adjustDistance :: World -> World
              adjustDistance w = w { wRobot = rob { rDistance = calculateDistance (wWalls w) (getSightLine 1 origin theta) origin } }
                 where rob = wRobot w
                       theta = rTheta rob
                       origin = rCoord rob
              calculateDistance :: [G.Point] -> Seg -> G.Point -> Float
              calculateDistance ws (p0, p1) origin = delimiter $ mapMaybe mapper (toSegments ws)
                 where mapper :: Seg -> Maybe G.Point
                       mapper (p2, p3) = intersectSegSeg p0 p1 p2 p3
                       toSegments :: [G.Point] -> [Seg]
                       toSegments = concatMap (getSegments 0.5)
                       delimiter :: [ G.Point ] -> Float
                       delimiter [] = maxDistSensor
                       delimiter ls = minimum (map (distance origin) ls)
              -- Updates the current Line value
              adjustRLine :: World -> World
              adjustRLine w = w { wRobot = (wRobot w) { rLine = calculateRLine (wLines w) } }
              calculateRLine :: [Seg] -> Line
              calculateRLine ls = lineFromBools $ mapTuple mapper sensorSeg
                 where sensorSeg@(sp1, sp2) = getLineSensorPoints 10 (rCoord r) (rTheta r)
                       mapper :: G.Point -> Bool
                       mapper p = any testIfOnLine (filter testIfLineInterSects ls)
                       -- Test if a sensorpoint is on the given line
                       testIfOnLine (lp1, lp2) = distance p (closestPointOnLine lp1 lp2 p) < lineThickness / 2
                       p = rCoord r
                       r = wRobot world
                       -- Test if sensor is somewhere on a line
                       testIfLineInterSects l@(p1, p2) = isJust (intersectSegSeg p1 p2 sp1 sp2)


              -- Updates the MVars sensor values and executes a Command if present
              applyDevToWorld :: Device -> World -> IO World
              applyDevToWorld (Device command distance line) w = do
                     maybeCommand <- tryTakeMVar command
                     _ <- tryTakeMVar distance
                     _ <- tryPutMVar distance (rDistance (wRobot world))
                     _ <- tryTakeMVar line
                     _ <- tryPutMVar line (rLine (wRobot world))
                     executeCommand maybeCommand w

-- Executes the given Command based on pattern matching a constructor
executeCommand :: Maybe Command -> World -> IO World
executeCommand Nothing w = return w
executeCommand (Just (SetRGB i r g b)) w@World { wRobot = rob } = return w { wRobot = rob { rLEDs = changeLED (rLEDs rob) i r g b } }
     where  changeLED :: (LED, LED) -> Int -> Int -> Int -> Int -> (LED, LED)
            changeLED (_, l) 1 r g b = ((r, g, b), l)
            changeLED (l, _) 2 r g b = (l, (r, g, b))
executeCommand (Just (SetMotor vl vr)) w@World { wRobot = rob } = return w { wRobot = rob { rVl = vl, rVr = vr } }

