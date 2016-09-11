import Forces.ThreeD

bodyForce :: [Force] -> [Force] -> Float -> [Force]
bodyForce ps vs dt = zipWith vectsum vs (map oneparticlev ps)
  where oneparticlev = \x -> mul dt $ foldr1 vectsum $ map (vforce x) ps

integrate :: [Force] -> [Force] -> Float -> [Force]
integrate ps vs dt = zipWith vectsum ps $ map (mul dt) vs

main = do
  input <- getLine
  let xs = words input
      n  = read $ head xs :: Int
      dt = read $ head $ tail xs :: Float
      i  = read $ head $ tail $ tail xs :: Float
  pi <- sequence $ map (const getLine) [1..n]
  vi <- sequence $ map (const getLine) [1..n]
  let ps = map toForce pi
      vs = map toForce vi
  print $ ps
  return ()
