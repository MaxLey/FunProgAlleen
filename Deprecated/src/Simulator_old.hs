module Simulator where
import Data.List (delete, sort)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Data.Bitmap (loadBMP)
import System.Environment (getArgs)

data World = World { wBorder :: Coord,
                     wRobot :: Robot,
                     wWalls :: [Wall],
                     wLines :: [Line]
                   } deriving (Eq, Ord, Show)

emptyWorld = World (20,20) initRobot [] []
initRobot = Robot (0,0) 0

type Direction    = Float

type X = Float
type Y = Float
type Coord = (X, Y)

data Robot   = Robot Coord Direction      
    deriving (Eq, Ord, Show)

data Wall   = Wall Coord
    deriving (Eq, Ord, Show)

data Line   = Line (Coord, Coord)
    deriving (Eq, Ord, Show)


(.-) , (.+) :: Coord -> Coord -> Coord
(x,y) .- (u,v) = (x-u,y-v)
(x,y) .+ (u,v) = (x+u,y+v)

(.*) :: Float -> Coord -> Coord
s .* (u,v) = (s*u,s*v)

infixl 6 .- , .+
infixl 7 .*

norm :: Coord -> Coord
norm (x,y) = let m = magV (x,y) in (x/m,y/m)

magV :: Coord -> Float
magV (x,y) = sqrt (x**2 + y**2) 

limitMag :: Float -> Coord -> Coord
limitMag n pt = if (magV pt > n) 
                  then n .* (norm pt)
                  else pt

rotateV :: Float -> Coord -> Coord
rotateV r (x,y) = (x * cos r - y * sin r
                  ,x * sin r + y * cos r)

parseWorld :: [String] -> World
parseWorld w = foldr (uncurry add) emptyWorld (withCoords w)

readWorld :: String -> IO World
readWorld f = parseWorld . lines <$> readFile f


simulateWorld :: Float -> (World -> World)

simulateWorld timeStep (World rocks (Robot robotPos shipV) bullets) 
  | any (collidesWith robotPos) rocks = GameOver
  | otherwise = World (concatMap updateRock rocks) 
                              (Robot newShipPos shipV)
                              (concat (map updateBullet bullets))
  where
      collidesWith :: Coord -> Wall -> Bool
      collidesWith p (Rock rp s _) 
       = magV (rp .- p) < s 

      collidesWithBullet :: Rock -> Bool
      collidesWithBullet r 
       = any (\(Bullet bp _ _) -> collidesWith bp r) bullets 
     
      updateRock :: Rock -> [Rock]
      updateRock r@(Rock p s v) 
       | collidesWithBullet r && s < 7 
            = []
       | collidesWithBullet r && s > 7 
            = splitRock r
       | otherwise                     
            = [Rock (restoreToScreen (p .+ timeStep .* v)) s v]
 
      updateBullet :: Bullet -> [Bullet] 
      updateBullet (Bullet p v a) 
        | a > 5                      
             = []
        | any (collidesWith p) rocks 
             = [] 
        | otherwise                  
             = [Bullet (restoreToScreen (p .+ timeStep .* v)) v 
                       (a + timeStep)] 

      newShipPos :: Coord
      newShipPos = restoreToScreen (robotPos .+ timeStep .* shipV)

splitRock :: Rock -> [Rock]
splitRock (Rock p s v) = [Rock p (s/2) (3 .* rotateV (pi/3)  v)
                         ,Rock p (s/2) (3 .* rotateV (-pi/3) v) ]

restoreToScreen :: Coord -> Coord
restoreToScreen (x,y) = (cycleCoordinates x, cycleCoordinates y)

cycleCoordinates :: (Ord a, Num a) => a -> a
cycleCoordinates x 
    | x < (-400) = -400
    | x > 400    = 400
    | otherwise  = x

drawWorld :: World -> Picture

drawWorld (World rocks (Robot (x,y) (vx,vy)) bullets)
  = pictures [ship, asteroids,shots]
   where 
    ship      = color red (pictures [translate x y (circle 10)])
    asteroids = pictures [translate x y (color orange (circle s)) 
                         | Rock   (x,y) s _ <- rocks]
    shots     = pictures [translate x y (color red (circle 2)) 
                         | Bullet (x,y) _ _ <- bullets]

handleEvents :: Event -> World -> World

handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos)
             (World rocks (Robot robotPos shipVel) bullets)
             = World rocks (Robot robotPos newVel) 
                          (newBullet : bullets)
 where 
     newBullet = Bullet robotPos Errors
                        (-150 .* norm (robotPos .- clickPos)) 
                        0
     newVel    = shipVel .+ (50 .* norm (robotPos .- clickPos))

handleEvents _ w = w

main = play 
         (InWindow "Asteroids!" (550,550) (20,20)) 
         black 
         24 
         initialWorld 
         drawWorld 
         handleEvents
         simulateWorld
