module Vector3 where

data Vector3 a = Vector3 { vecX :: a, 
                           vecY :: a, 
                           vecZ :: a,
                           vecM :: a } deriving (Show, Eq)

instance Num a => Num (Vector3 a) where
  (Vector3 x1 y1 z1 _) + (Vector3 x2 y2 z2 _) = vector3 x y z
      where
        x = x1 + x2
        y = y1 + y2
        z = z1 + z2
  (Vector3 x1 y1 z1 _) * (Vector3 x2 y2 z2 _) = vector3 x y z
      where
        x = x1 * x2
        y = y1 * y2
        z = z1 * z2
  (Vector3 x1 y1 z1 _) - (Vector3 x2 y2 z2 _) = vector3 x y z
      where
        x = x1 - x2
        y = y1 - y2
        z = z1 - z2
  negate (Vector3 x y z m) = Vector3 (negate x) (negate y) (negate z) m
  abs (Vector3 x y z m) = Vector3 (abs x) (abs y) (abs z) m
  signum (Vector3 x y z m) = Vector3 (signum x) (signum y) (signum z) m
  fromInteger a = vector3 (fromInteger a) (fromInteger a) (fromInteger a)

vector3 x y z = Vector3 x y z (x*x + y*y + z*z)      

origin, xAxis, yAxis, zAxis :: Num a => Vector3 a
origin = vector3 0 0 0
xAxis  = vector3 1 0 0
yAxis  = vector3 0 1 0
zAxis  = vector3 0 0 1

dot :: Num a => Vector3 a -> Vector3 a -> a
dot (Vector3 x1 y1 z1 _) (Vector3 x2 y2 z2 _) = x1*x2 + y1*y2 + z1*z2

magSq :: Num a => Vector3 a -> a
magSq (Vector3 x y z m) = m

mag :: (Floating a, Num a) => Vector3 a -> a
mag = sqrt . magSq

cross :: Num a => Vector3 a -> Vector3 a -> Vector3 a
cross (Vector3 x1 y1 z1 _) (Vector3 x2 y2 z2 _) = Vector3 x y z m
    where
      x = y1*z2 - z1*y2
      y = z1*x2 - x1*z2
      z = x1*y2 - y1*x1
      m = x*x + y*y + z*z

scale :: (Fractional a, Num a) => a -> Vector3 a -> Vector3 a        
scale s (Vector3 x y z m) = Vector3 x' y' z' (m/(s*s))
    where
      x' = x/s
      y' = y/s
      z' = z/s

setMag :: (Floating a, Num a) => a -> Vector3 a -> Vector3 a
setMag m v = scale (m/mag v) v

unit :: (Floating a, Num a) => Vector3 a -> Vector3 a      
unit = setMag 1
