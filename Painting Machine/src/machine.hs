lineAnalyzer :: [Char] -> [(Int,Int)]
lineAnalyzer xs = zip (lineStarts xs) (lineEnds xs)
  where
    indexList  = \x -> zip [0..] $ listOfTwo x
    takeTwo    = \x -> head x : head (tail x) : []
    lineStarts = \x -> if head x == '#' then 0 : f x else f x
    lineEnds   = \x -> if last x == '#' then g x ++ [length xs - 1] else g x
    listOfTwo  = \x -> if length x == 1 then [x] else takeTwo x : listOfTwo (tail x)
    g          = \x -> foldr (\x acc -> if end (snd x) then (fst x) : acc else acc) [] $ indexList x
    start      = \x -> if x == [] || length x <= 1 then False else head x == '.' && head (tail x) == '#'
    end        = \x -> if x == [] || length x <= 1 then False else head x == '#' && head (tail x) == '.'
    f          = \x -> foldr (\x acc -> if start (snd x) then (fst x + 1) : acc else acc) [] $ indexList x
