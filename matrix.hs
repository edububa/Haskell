matrixmul :: [[Float]] -> [[Float]] -> [[Float]]
matrixmul xs ys = map (\x -> map (dotprod x) (transpose ys)) xs

transpose :: [[a]] -> [[a]]
transpose xs = foldr (zipWith (:)) (map (const []) [1..(length xs)]) xs 
          
dotprod :: [Float] -> [Float] -> Float
dotprod xs ys = foldr (+) 0 $ zipWith (*) xs ys
