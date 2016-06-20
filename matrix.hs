matrixmul :: [[Float]] -> [[Float]] -> [[Float]]
matrixmul xs ys = map (mulRow ys) xs

mulRow :: [[Float]] -> [Float] -> [Float]
mulRow ys x = map (dotprod x) (transpose ys)

transpose :: [[a]] -> [[a]]
transpose xs = foldr (zipWith (:)) (emptyColumn (length (head xs))) xs

emptyColumn :: Int -> [[a]]
emptyColumn n = map (const []) [1 .. n]

dotprod :: [Float] -> [Float] -> Float
dotprod xs ys = foldr (+) 0 (zipWith (*) xs ys)
