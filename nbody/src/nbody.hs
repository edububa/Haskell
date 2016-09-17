import Forces.ThreeD

bodyForce :: ([Force], [Force], Float) -> ([Force], [Force], Float)
bodyForce (ps, vs, dt) = (ps, newForce, dt)
  where
    oneparticlev = \x -> mul dt $ foldr1 vectsum $ map (vforce x) ps
    newForce     = zipWith vectsum vs (map oneparticlev ps)

integrate :: ([Force], [Force], Float) -> ([Force], [Force], Float)
integrate (ps,vs,dt) = (newPos, vs, dt)
  where newPos = zipWith vectsum ps $ map (mul dt) vs

iterations :: Int -> [Force] -> [Force] -> Float -> [([Force], [Force], Float)]
iterations n ps vs dt = take n $ iterate (integrate . bodyForce) (ps, vs, dt)

positions :: [([Force], [Force], Float)] -> [[Force]]
positions xs = foldr (\(ps,_,_) acc -> ps : acc) [] xs

main :: IO ()
main = do
  input <- getLine
  let xs = words input
      n  = read $ head xs :: Int
      dt = read $ head $ tail xs :: Float
      i  = read $ head $ tail $ tail xs :: Int
  pi <- sequence $ map (const getLine) [1..n]
  vi <- sequence $ map (const getLine) [1..n]
  let ps = map toForce pi
      vs = map toForce vi
  print $ positions $ iterations i ps vs dt
  return ()
