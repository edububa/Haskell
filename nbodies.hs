-- n bodies simulation
-- type definitions
type Position = (Float, Float)
type Vector   = (Float, Float)

-- functions
distance :: Position -> Position -> Float
distance p1 p2 = sqrt ((fst p2 - fst p1)^2 + (snd p2 - snd p1)^2)

force :: Position -> Position -> Float
force p1 p2
  | p1 == p2  = 0
  | otherwise = g / (distance p1 p2)^2
  where g = 6.67408 * 1.0e-11
          
vforce :: Position -> Position -> Vector
vforce p1 p2
  | p1 == p2 = (0, 0)
  | otherwise = (f * (v1 / modv), f * (v2 / modv))
  where f     = force p1 p2
        modv  = distance p1 p2
        v1    = (fst p2 - fst p1)
        v2    = (snd p2 - snd p1)

vectorsum :: Vector -> Vector -> Vector
vectorsum (v1, v2) (w1, w2) = (v1 + v2, w1 + w2)

onetoallpart :: Position -> [Position] -> Vector
onetoallpart x ys = foldr vectorsum (0,0) (map (vforce x) ys)
