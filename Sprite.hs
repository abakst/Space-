module Sprite where

import Graphics.UI.SDL
import Graphics.UI.SDL.Rotozoomer

data Sprite = Sprite Surface

loadSprite :: String -> IO Sprite
loadSprite fpath = do
  image <- loadBMP fpath
  return $ Sprite image
  
scaleSprite :: Sprite -> Double -> IO Sprite  
scaleSprite (Sprite sprite) scale = do
  sprite' <- rotozoom sprite 0 scale True
  return $ Sprite sprite'
  
spriteWidth (Sprite sprite) = surfaceGetWidth sprite
spriteHeight (Sprite sprite) = surfaceGetHeight sprite
  
drawSprite :: Sprite -> Int -> Int -> Double -> Surface -> IO Bool 
drawSprite (Sprite sprite) x y rot dest = do
  im <- rotozoom sprite rot 1 True
  let x' = x-(surfaceGetWidth im)`div`2
  let y' = y-(surfaceGetHeight im)`div`2
  blitSurface im Nothing dest (Just (Rect x' y' 0 0))