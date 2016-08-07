matrixmul :: [[Float]] -> [[Float]] -> [[Float]]
matrixmul xs ys = map mulRow xs
  where mulRow = \x -> map (dotprod x) (transpose ys)
  
transpose :: [[a]] -> [[a]]
transpose xs = foldr (zipWith (:)) (emptyColumn (length (head xs))) xs
  where emptyColumn = \x -> map (const []) [1..x]
          
dotprod :: [Float] -> [Float] -> Float
dotprod xs ys = foldr (+) 0 (zipWith (*) xs ys)
