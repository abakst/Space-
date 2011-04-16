{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module KdTree where

import Data.Function
import Data.List
import Data.Ord

import Zoepis.ZVector

data KdTree a = KdTree Int a (KdTree a) (KdTree a) | Nil
              deriving Show

class Num a => KdPoint p a where
    dim        :: p a -> Int
    sub        :: p a -> Int -> a
    distAxisSq :: p a -> p a -> Int -> a
    distSq     :: p a -> p a -> a
    
-- heh, gross...    
instance KdPoint Vector3D Double where
    dim p = 3
    sub p n = case n of
                0 -> vecX p
                1 -> vecY p
                2 -> vecZ p
                _ -> 0
    distAxisSq pt1 pt2 i = let d = sub pt1 i - sub pt2 i in d*d
    distSq pt1 pt2       = zMagSq (pt1 - pt2) 
    
kdTree :: (KdPoint p a, Ord a) => [p a] -> KdTree (p a)    
kdTree ps = build 0 ps
    where
      build :: (KdPoint p a, Ord a) => Int -> [p a] -> KdTree (p a)
      build d [] = Nil
      build d ps = KdTree d (head geq) (build d' less) (build d' (tail geq))
          where
            axis   = mod d . dim $ head ps
            sorted = sortBy (comparing $ flip sub axis) ps
            median = length sorted `div` 2
            (less, geq) = splitAt median sorted
            d'     = succ d            
            
kdNN :: (KdPoint p a, Ord a, Floating a) => KdTree (p a) -> p a -> KdTree (p a)
kdNN tree pt = kdNN' tree tree
    where
      kdNN' Nil best = best
      kdNN' t@(KdTree axis p l r) best =
          let (near, far) = orderChildren pt t
              best' = kdNN' near best
              best'' = if kdDist t pt < kdDist best' pt
                          then t else best'
          in if kdDistAxis t pt < kdDist best'' pt
                then kdNN' far best'' else best''
                                           
orderChildren pt t@(KdTree axis p l Nil) = (l, Nil)
orderChildren pt t@(KdTree axis p Nil r) = (r, Nil)
orderChildren pt t@(KdTree axis p l r) = 
 if sub pt axis < sub p axis
 then (l, r)
 else (r, l)
  
{-  if distAxisSq l pt < kdDistAxis r pt
     then (l, r) 
     else (r, l) -}
                                           
kdNNs :: (KdPoint p a, Ord a, Floating a) =>
        KdTree (p a) -> p a -> a -> [p a]
kdNNs tree pt rad = kdNNs' tree []
    where
      kdNNs' Nil ns = ns
      kdNNs' t@(KdTree axis p l r) ns =
        let (near, far) = orderChildren pt t
            ns' = kdNNs' near ns
            ns'' = if kdDist t pt <= rad*rad
                      then p:ns'
                      else ns'
        in if kdDistAxis t pt <= rad*rad
              then kdNNs' far ns''
              else ns''
  
kdDist :: (KdPoint p a, Floating a) => KdTree (p a) -> p a -> a
kdDist Nil _ = undefined
kdDist (KdTree _ p _ _) pt = distSq p pt

kdDistAxis :: (KdPoint p a, Floating a) => KdTree (p a) -> p a -> a
kdDistAxis Nil _ = undefined
kdDistAxis (KdTree axis p _ _) pt = distAxisSq p pt axis
