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

width = 800
height = 600
n = 5
ticks = 30


main = do SDL.init [InitVideo, InitAudio]
          openAudio 44100 AudioS16Sys 2 4096
          screen <- setVideoMode width height 16 [SWSurface]
          setCaption "Test" ""
          enableUnicode True
          ship <- loadSprite "./res/ship.bmp" >>= Prelude.flip scaleSprite 0.7
          music <- loadMUS "./res/music.mp3"
          playMusic music (-1)
          game <- build n setup $ Game {
                    boids = [],
                    music = music,
                    mode = Paused,
                    sprite = ship
                  }
          gameLoop $ game { mode = Running }
          
build 0 f a = return a           
build n f a = do a' <- f a
                 build (pred n) f a'
                             
data Game = Game {                 
      boids :: [Boid],
      music :: Music,
      mode :: GameMode,
      sprite :: Sprite
  } 
            
data GameMode = Paused | Running | Finished deriving (Eq)
         
randomBoid :: Double -> IO Boid         
randomBoid s = do
  x <- randomRIO(width`div`4, 5*width`div`6)
  y <- randomRIO(height`div`4, 5*height`div`6)
  theta <- randomRIO(0, 2*pi)
  let vx = s * cos theta
  let vy = s * sin theta
  return $ Boid { boidI = 0,
                  boidP = vector3D (fromIntegral x, fromIntegral y, 0),
                  boidV = vector3D (vx, vy, 0),
                  boidA = NoAction }
         
forward = vector3D (0,-1,0) :: Vector          
angle :: Vector -> Vector -> Double
angle v1 v2 = let x1 = vecX v1
                  x2 = vecX v2
                  y1 = vecY v1
                  y2 = vecY v2
              in atan2 y2 x2 - atan2 y1 x1
                                            
drawBoid spr screen b = 
  let posX = floor $ vecX p
      posY = floor $ vecY p
      ang = angle forward (unit v)
      p = boidP b
      v = boidV b
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
  
bx = Boids.separation 1 15
   ->> Boids.cohesion 0.05 100
   ->> Boids.alignment 0.125 80
   ->> Boids.stayInBounds 0.5 20 0.0 0.0
                   (fromIntegral width) (fromIntegral height)

gameLoop :: Game -> IO ()
gameLoop game = do
  delay ticks
  event <- pollEvent
  let tree = kdTree . boids $ game
  case event of
    Quit -> exitWith ExitSuccess
    KeyDown (Keysym _ _ 'q') -> exitWith ExitSuccess
    KeyDown (Keysym SDLK_RETURN _ _) ->
        if mode game == Paused
        then do resumeMusic
                gameLoop game { mode = Running }
        else do pauseMusic
                gameLoop game { mode = Paused }
    _ -> return ()
  display (sprite game) (boids game)
  case mode game of
--    Setup -> setup game >>= gameLoop 
    Paused -> gameLoop game
    Running ->
        gameLoop $ game { boids = map (moveBoid bx 0.1 tree) (boids game) }
    Finished -> gameLoop game
    
setup :: Game -> IO Game
setup game = position >>= direction xAxis >>= addBoid
    where position = do
            event <- waitEvent
            case event of
              (MouseButtonDown x y _) -> return $ vec x y
              Quit -> exitWith ExitSuccess
              _ -> position
          direction dir pos = do
            display (sprite game) (newBoid pos dir:boids game)
            event <- waitEvent
            case event of
              (MouseButtonUp x y _) -> return (pos, vec x y - pos)
              (MouseMotion x y _ _) -> direction (vec x y - pos) pos
              Quit -> exitWith ExitSuccess
              _ -> direction dir pos
          addBoid (pos, dir) = do
                       let b = newBoid pos dir
                       return $ game { boids = b:(boids game) }
          newBoid pos dir = Boid { boidI = length (boids game),
                                   boidA = NoAction,
                                   boidP = pos,
                                   boidV = 15 * unit dir
                                 }
          vec x y = vector3D (fromIntegral x, fromIntegral y, 0)
          