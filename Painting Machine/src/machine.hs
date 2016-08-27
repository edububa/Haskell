import Data.List.Split

indexList :: [Char] -> [(Int, [Char])]
indexList xs = zip [0..] $ listOfTwo xs
  where
    listOfTwo  = \x -> if length x == 1 then [x] else takeTwo x : listOfTwo (tail x)
    takeTwo    = \(x:xs) -> x : head xs : []

lineStarts :: [Char] -> [Int]
lineStarts xs = if head xs == '#' then 0 : f xs else f xs
  where
    start = \x -> x /= [] && length x > 1 && head x == '.' && head (tail x) == '#'
    f     = \x -> foldr (\x acc -> if start (snd x) then (fst x + 1) : acc else acc) [] $ indexList x

lineEnds :: [Char] -> [Int]
lineEnds xs = if last xs == '#' then g xs ++ [length xs - 1] else g xs
  where
    end = \x -> x /= [] && length x > 1 && head x == '#' && head (tail x) == '.'
    g   = \x -> foldr (\x acc -> if end (snd x) then (fst x) : acc else acc) [] $ indexList x
    
lineAnalyzer :: [Char] -> [(Int,Int)]
lineAnalyzer xs = zip (lineStarts xs) (lineEnds xs)

inputAnalyzer ::[Char] -> [(Int, [(Int, Int)])]
inputAnalyzer xs = zip [0..] $ map lineAnalyzer (splitOn "\n" xs)
