module Main where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Rotozoomer
import Graphics.UI.SDL.Primitives
import Graphics.UI.SDL.Mixer

import Foreign
import Data.Typeable
import Data.Char
import Debug.Trace

import System.Environment
import System.Exit
import System.Random

import Sprite
import Boids
import KdTree
import Zoepis.ZVector
import Control.Monad

width = 1024
height = 768
n = 25

main = do SDL.init [InitVideo, InitAudio]
          openAudio 44100 AudioS16Sys 2 4096
          screen <- setVideoMode width height 16 [SWSurface]
          setCaption "Test" ""
          enableUnicode True
          ship <- loadSprite "./res/ship.bmp" >>= Prelude.flip scaleSprite 0.7
          bs <- replicateM n (randomBoid 20)
          let boids = zipWith (\n (Boid _ (p,v)) -> Boid n (p, v)) [0..] bs
--          let boids = [Boid 0 (vector3D (400, 350, 0), 15*xAxis),
--                       Boid 1 (vector3D (900, 364, 0), (-15)*xAxis)]
          music <- loadMUS "./res/music.mp3"
          playMusic music (-1)
          loop music boids (display ship)
         
randomBoid :: Double -> IO Boid         
randomBoid s = do
  x <- randomRIO(width`div`4, 5*width`div`6)
  y <- randomRIO(height`div`4, 5*height`div`6)
  theta <- randomRIO(0, 2*pi)
  let vx = s * cos theta
  let vy = s * sin theta
  return $ Boid 0 (vector3D (fromIntegral x, fromIntegral y, 0),
                   vector3D (vx, vy, 0))
         
forward = vector3D (0,-1,0) :: Vector          
angle :: Vector -> Vector -> Double
angle v1 v2 = let x1 = vecX v1
                  x2 = vecX v2
                  y1 = vecY v1
                  y2 = vecY v2
              in atan2 y2 x2 - atan2 y1 x1
                                            
drawBoid spr screen (Boid _ (p, v)) = 
  let posX = floor $ vecX p
      posY = floor $ vecY p
      ang = angle forward (unit v)
  in do
    let format = surfaceGetPixelFormat screen
    drawSprite spr posX posY ((-ang)*180/pi) screen
                                            
display :: Sprite -> [Boid] -> IO ()          
display sprite boids = do
  screen <- getVideoSurface
  let format = surfaceGetPixelFormat screen
  grey <- mapRGB format 0x05 0x05 0x05
  fillRect screen Nothing grey
  mapM_ (drawBoid sprite screen) boids
  SDL.flip screen
  
ticks = 30
loop :: Music -> [Boid] -> ([Boid] -> IO ()) -> IO ()
loop music boids display = do
  delay ticks
  event <- pollEvent
  let tree = kdTree boids
  case event of
    Quit -> exitWith ExitSuccess
    KeyDown (Keysym _ _ 'q') -> exitWith ExitSuccess
    _ -> return ()
  display boids >> loop music (map (moveBoid bx 0.1 tree) boids) display

bx = Boids.separation 0.8 20
   ->> Boids.cohesion 0.01 80
   ->> Boids.alignment 0.1 80
   ->> Boids.stayInBounds 0.5 20 0.0 0.0
                   (fromIntegral width) (fromIntegral height)