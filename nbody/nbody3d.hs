type Triple = (Float,Float,Float)

mul :: Float -> Triple -> Triple
mul n (x,y,z) = (n * x, n * y, n * z)

tsum :: Triple -> Triple -> Triple
tsum (x1,x2,x3) (y1,y2,y3) = (x1 + y1, x2 + y2, x3 + y3)

distance :: Triple -> Triple -> Float
distance (x1,x2,x3) (y1,y2,y3) = sqrt $ (y1 - x1)^2 + (y2 - x2)^2 + (y3 - x3)^2

vforce :: Triple -> Triple -> Triple
vforce p1@(x1,x2,x3) p2@(y1,y2,y3)
  | p1 == p2  = (0,0,0)
  | otherwise = (f * (v1 / modv), f * (v2 / modv), f * (v3/modv))
  where f     = (6.67408 * 1.0e-11) / (distance p1 p2)^2
        modv  = distance p1 p2
        v1    = y1 - x1
        v2    = y2 - x2
        v3    = y3 - x3

bodyForce :: [Triple] -> [Triple] -> Float -> [Triple]
bodyForce ps vs dt = zipWith tsum vs (map oneparticlev ps)
  where oneparticlev = \x -> mul dt $ foldr tsum (0,0,0) $ map (vforce x) ps

integrate :: [Triple] -> [Triple] -> Float -> [Triple]
integrate ps vs dt = zipWith tsum ps $ map (mul dt) vs
