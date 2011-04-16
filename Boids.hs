{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module Boids where

import Zoepis.ZVector
import KdTree
import Data.List
import Control.Arrow
import Debug.Trace

type Position = Vector3D Double
type Vector   = Vector3D Double

data Boid' a = Boid Int (Vector3D a, Vector3D a) deriving Show
type Boid = Boid' Double
data Boids = Boids Int [Boid]

instance KdPoint Boid' Double where
  dim (Boid _ (p, _)) = dim p
  sub (Boid _ (p, _)) i = sub p i
  distAxisSq (Boid _ (p1, _)) (Boid _ (p2, _)) i = distAxisSq p1 p2 i
  distSq (Boid _ (p1, _)) (Boid _ (p2, _)) = distSq p1 p2
  
newtype BoidRule a = BR { runRule :: KdTree Boid -> Boid -> a }

(->>) (BR r1) (BR r2) = BR (\tree boid -> (r1 tree boid) + (r2 tree boid))

--close :: Num a => a       
--close = fromIntegral 80
--tooclose :: Num a => a        
--tooclose = fromIntegral 20
maxAccel = 0.2
maxSpeed = 20

boidV (Boid _ (_, v)) = v
boidP (Boid _ (p, _)) = p
boidI (Boid i _) = i

boidAngle (Boid _ (p1, v)) (Boid _ (p2, _)) = 
  let dp = unit v `zDot` unit (p2 - p1)
  in acos dp

accelBoid :: Boid -> Vector -> Boid
accelBoid (Boid id (p, v)) v' =
    if magv' > 0
    then
        clampBoidV maxSpeed . Boid id $ (p, vNew)
    else
        Boid id (p, v)
    where
      magv = sqrt(zMagSq v)
      magv' = sqrt(zMagSq v')
      vNew = v + scale (maxSpeed*maxAccel) v'
      
clampBoidV :: Double -> Boid -> Boid
clampBoidV l (Boid id (p, v))  = Boid id (p, clampVector l v)

clampVector :: Double -> Vector -> Vector
clampVector val vec = if zMagSq vec > val*val
                         then setMag val vec
                         else vec
                              
stepBoid :: Double -> Boid -> Boid 
stepBoid dt (Boid i (p, v)) = Boid i (p + scale dt v,v)

moveBoid :: BoidRule Vector -> Double -> KdTree Boid -> Boid -> Boid
moveBoid br dt allBoids boid@(Boid i (p,v)) = 
  let acc = runRule br allBoids boid
  in stepBoid dt $ accelBoid boid acc
     
cohesion :: Double -> Double -> BoidRule Vector         
cohesion s close = BR $ f
    where
      f allBoids boid =
          let nns = filter (neighbor boid) $ kdNNs allBoids boid close
              (pAv, _ , n) = averageBoids nns
          in if n == 0 
             then origin
             else scale s $ unit $ pAv - boidP boid
             
alignment :: Double -> Double -> BoidRule Vector                  
alignment s close = BR f
    where
      f allBoids boid =
          let nns = filter (neighbor boid) $ kdNNs allBoids boid close
              (_, vAv, n) = averageBoids nns
          in if n == 0 
             then origin
             else scale s $ unit $ vAv - boidV boid
                  
separation :: Double -> Double -> BoidRule Vector
separation s tooclose = BR f
    where
      f allBoids boid = 
        let nns = filter (neighbor boid) $ kdNNs allBoids boid tooclose
            df b1 b2 = boidP b1 - boidP b2
        in if length nns == 0
           then origin
           else scale s $ unit $ foldl' (+) origin $ map (df boid) nns


invDiff x1 x2 = 1/sqrt(zMagSq (x1 - x2))           
invDist p1 p2 = 1/sqrt(zMagSq (p1 - p2))
invVec p1 p2 = scale (invDist p1 p2) (p1 - p2)
           
stayInBounds :: Double -> Double ->
               Double -> Double -> Double -> Double -> BoidRule Vector
stayInBounds k close x0 y0 x1 y1 = BR f
    where
      f allBoids boid = let x = vecX $ boidP boid
                            y = vecY $ boidP boid
                            vx0 = if x < x0 + (close/4)
                                    then xAxis
                                    else origin
                            vx1 = if x > x1 - (close/4)
                                     then (-xAxis)
                                     else origin
                            vy0 = if y < y0 + (close/4)
                                     then yAxis
                                     else origin
                            vy1 = if y > y1 - (close/4)
                                     then (-yAxis)
                                     else origin
                        in scale k (vx0 + vx1 + vy0 + vy1)

averageBoids :: [Boid] -> (Vector, Vector, Int)
averageBoids bs = 
  let (sumP, sumV) = foldl' (\(p,v) b -> (p + boidP b, v + boidV b))
                            (vector3D (0,0,0), vector3D (0,0,0))
                            bs
      n = length bs
  in if n == 0 
        then (vector3D (0,0,0), vector3D (0,0,0), n)
        else (scale (1.0 / fromIntegral n) sumP,
              scale (1.0 / fromIntegral n) sumV, n)
             
neighbor :: Boid -> Boid -> Bool
neighbor b1 b2 = (inVicinity b1 b2) && (not (sameBoid b1 b2))

inVicinity :: Boid -> Boid -> Bool
inVicinity b1 b2 = boidAngle b1 b2 < 3*pi/4

sameBoid :: Boid -> Boid -> Bool
sameBoid b = (== boidI b) . boidI
