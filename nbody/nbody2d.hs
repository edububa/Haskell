type DoubleF = (Float,Float)

mul :: Float -> DoubleF -> DoubleF
mul n (x,y) = (n * x, n * y)

tsum :: DoubleF -> DoubleF -> DoubleF
tsum (x1,x2) (y1,y2) = (x1 + y1, x2 + y2)

distance :: DoubleF -> DoubleF -> Float
distance (x1,x2) (y1,y2) = sqrt ((y1 - x1)^2 + (y2 - x2)^2)

vforce :: DoubleF -> DoubleF -> DoubleF
vforce p1@(x1,x2) p2@(y1,y2)
  | p1 == p2  = (0,0)
  | otherwise = (f * (v1 / modv), f * (v2 / modv))
  where f     = (6.67408 * 1.0e-11) / (distance p1 p2)^2
        modv  = distance p1 p2
        v1    = y1 - x1
        v2    = y2 - x2

bodyForce :: [DoubleF] -> [DoubleF] -> Float -> [DoubleF]
bodyForce ps vs dt = zipWith tsum vs (map oneparticlev ps)
  where oneparticlev = \x -> mul dt (foldr tsum (0,0) (map (vforce x) ps))

integrate :: [DoubleF] -> [DoubleF] -> Float -> [DoubleF]
integrate ps vs dt = zipWith tsum ps (map (mul dt) vs)
