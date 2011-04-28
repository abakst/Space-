{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module Boids where

--import Zoepis.ZVector
import Vector3
import KdTree
import Data.List
import Control.Arrow
import Debug.Trace

type Position = Vector3 Double
type Vector   = Vector3 Double

data Boid' a = Boid {
      boidI :: Int,
      boidP :: Vector3 a,
      boidV :: Vector3 a,
      boidA :: BoidAction
    } deriving Show
               
data BoidAction = NoAction
                | FireAt Int Int -- Target, Cooldown
                deriving Show             
               
type Boid = Boid' Double

instance KdPoint Boid' Double where
  dim = dim . boidP
  sub b i = flip sub i $ boidP b
  distAxisSq b1 b2 i = distAxisSq (boidP b1) (boidP b2) i
  distSq b1 b2 = distSq (boidP b1) (boidP b2)
  
newtype BoidRule a = BR { runRule :: KdTree Boid -> Boid -> a }

(->>) (BR r1) (BR r2) = BR (\tree boid -> (r1 tree boid) + (r2 tree boid))
                        
maxAccel = 0.1
maxSpeed = 25

boidAngle b1 b2 =
    let dp = (unit $ boidV b1) `dot` (unit $ boidP b2 - boidP b1)
    in acos dp

accelBoid :: Boid -> Vector -> Boid
accelBoid b v' = if magv' > 0
                 then
                     clampBoidV maxSpeed $ b { boidV = vNew }
                 else
                     b
    where
      magv = mag $ boidV b
      magv' = mag v'
      vNew = boidV b + clampVector (maxSpeed*maxAccel) v'
      
clampBoidV :: Double -> Boid -> Boid
clampBoidV l b  = b { boidV = clampVector l $ boidV b }

clampVector :: Double -> Vector -> Vector
clampVector val vec = if magSq vec > val*val
                         then setMag val vec
                         else vec
                              
moveBoid :: BoidRule Vector -> Double -> KdTree Boid -> Boid -> Boid
moveBoid br dt allBoids boid =
    let acc = runRule br allBoids boid
        moveStep dt b = b { boidP = boidP b + (scale dt $ boidV b) }
    in moveStep dt $ accelBoid boid acc
                                  
cohesion :: Double -> Double -> BoidRule Vector         
cohesion s close = BR $ f
    where
      f allBoids boid =
          let nns = filter (neighbor boid) $ kdNNs allBoids boid close
              (pAv, _ , n) = averageBoids nns
          in if n == 0 
             then origin
             else scale s $ {- unit $ -} pAv - boidP boid
             
alignment :: Double -> Double -> BoidRule Vector                  
alignment s close = BR f
    where
      f allBoids boid =
          let nns = filter (neighbor boid) $ kdNNs allBoids boid close
              (_, vAv, n) = averageBoids nns
          in if n == 0 
             then origin
             else scale s $ {- unit $ -} vAv - boidV boid
                  
separation :: Double -> Double -> BoidRule Vector
separation s tooclose = BR f
    where
      f allBoids boid = 
        let nns = filter (neighbor boid) $ kdNNs allBoids boid tooclose
            df b1 b2 = boidP b1 - boidP b2
        in if length nns == 0
           then origin
           else scale s $ {- unit $ -} foldl' (+) origin $ map (df boid) nns
                
{--
attack :: Double -> BoidRule BoidAction
attack range = BR f
    where
      f allBoids boid =
          let nns = filter (target boid) $ kdNNs allBoids boid range
              target b1 b2 = not (sameBoid b1 b2) && boidAngle b1 b2 < pi/20
--}

invDiff x1 x2 = 1/mag (x1 - x2)
invDist p1 p2 = 1/mag (p1 - p2)
invVec p1 p2 = scale (invDist p1 p2) (p1 - p2)
           
stayInBounds :: Double -> Double ->
               Double -> Double -> Double -> Double -> BoidRule Vector
stayInBounds k close x0 y0 x1 y1 = BR f
    where
      f allBoids boid = let (Vector3 x y _ _) = boidP boid
--                            y = vecY $ boidP boid
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
                            (vector3 0 0 0, vector3 0 0 0)
                            bs
      n = length bs
  in if n == 0 
        then (vector3 0 0 0, vector3 0 0 0, n)
        else (scale (1.0 / fromIntegral n) sumP,
              scale (1.0 / fromIntegral n) sumV, n)
             
neighbor :: Boid -> Boid -> Bool
neighbor b1 b2 = (inVicinity b1 b2) && (not (sameBoid b1 b2))

inVicinity :: Boid -> Boid -> Bool
inVicinity b1 b2 = boidAngle b1 b2 < 3*pi/4

sameBoid :: Boid -> Boid -> Bool
sameBoid b = (== boidI b) . boidI
