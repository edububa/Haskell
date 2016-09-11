module Forces.TwoD
  ( mul
  , vectsum
  , distance
  , vforce
  , Force
  , toForce
  ) where

type Force = (Float,Float)

mul :: Float -> Force -> Force
mul n (x,y) = (n * x, n * y)

vectsum :: Force -> Force -> Force
vectsum (x1,x2) (y1,y2) = (x1 + y1, x2 + y2)

distance :: Force -> Force -> Float
distance (x1,x2) (y1,y2) = sqrt ((y1 - x1)^2 + (y2 - x2)^2)

vforce :: Force -> Force -> Force
vforce p1@(x1,x2) p2@(y1,y2)
  | p1 == p2  = (0,0)
  | otherwise = (f * (v1 / modv), f * (v2 / modv))
  where f     = (6.67408 * 1.0e-11) / (distance p1 p2)^2
        modv  = distance p1 p2
        v1    = y1 - x1
        v2    = y2 - x2

toForce :: String -> Force
toForce xs = (head l, last l)
  where l = map read $ words xs :: [Float]
