module Forces.ThreeD
  ( mul
  , vectsum
  , distance
  , vforce
  , Force
  , toForce
  ) where

type Force = (Float,Float,Float)

mul :: Float -> Force -> Force
mul n (x,y,z) = (n * x, n * y, n * z)

vectsum :: Force -> Force -> Force
vectsum (x1,x2,x3) (y1,y2,y3) = (x1 + y1, x2 + y2, x3 + y3)

distance :: Force -> Force -> Float
distance (x1,x2,x3) (y1,y2,y3) = sqrt $ (y1 - x1)^2 + (y2 - x2)^2 + (y3 - x3)^2

vforce :: Force -> Force -> Force
vforce p1@(x1,x2,x3) p2@(y1,y2,y3)
  | p1 == p2  = (0,0,0)
  | otherwise = (f * (v1 / modv), f * (v2 / modv), f * (v3 / modv))
  where f     = (6.67408 * 1.0e-11) / (distance p1 p2)^2
        modv  = distance p1 p2
        v1    = y1 - x1
        v2    = y2 - x2
        v3    = y3 - x3

toForce :: String -> Force
toForce xs = (head l, head $ tail l, last l)
  where l = map read $ words xs :: [Float]
