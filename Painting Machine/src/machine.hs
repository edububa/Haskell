
linesStarts :: [Char] -> [Int]
linesStarts (x:xs)
  | x == '#'  = [0] ++ linesStartsAux xs 1
  | otherwise = linesStartsAux xs 1

linesStartsAux :: [Char] -> Int -> [Int]
linesStartsAux xss@(x1:x2:xs) y
  | xss == []   = []
  | xss == "##" = []
  | xss == ".." = []
  | x1  == '.' && x2 == '#' = [y + 1] ++ linesStartsAux (x2:xs) (y + 1)
  | otherwise = linesStartsAux (x2:xs) (y + 1)

linesEnds :: [Char] -> [Int]
linesEnds xs = linesEndsAux xs 0

linesEndsAux :: [Char] -> Int -> [Int]
linesEndsAux xss@(x1:x2:xs) y
  | xss == []   = []
  | xss == ".#" = [y]
  | xss == "##" = [y + 1]
  | x1  == '#' && x2 == '.' = [y] ++ linesEndsAux (x2:xs) (y + 1)
  | otherwise = linesEndsAux (x2:xs) (y + 1)

lineAnalyzer :: [Char] -> [(Int,Int)]
lineAnalyzer xs = zip (linesStarts xs) (linesEnds xs)

toMatrix :: Int -> Int -> [Char] -> [(Int,Int,Int,Int)]
toMatrix r c xs = map to2dim (lineAnalyzer xs)
  where to2dim  = \(x1,x2) -> (div x1 c, mod x1 c, div x2 c, mod x2 c)
