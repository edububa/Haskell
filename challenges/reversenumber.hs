-- challenge 70: reverse a number
reversenumber :: Int -> Int
reversenumber x = reversenumberaux x (ncifras x)

ncifras :: Int -> Int
ncifras x =
  if div x 10 == mod x 10 then 0
  else 1 + ncifras(div x 10)
  
reversenumberaux :: Int -> Int -> Int
reversenumberaux x n =
  if (n - 1) == 0 then x
  else (mod x 10) * 10 ^ (n - 1) + reversenumberaux (div x 10) (n - 1)

-- other implementation
reversenumberf :: Int -> Int
reversenumberf x =
  foldr (+) (0) (zipWith (*) (reverse (map (10^) [0..((length (splitrevnumber x)) - 1)])) (splitrevnumber x))

splitrevnumber :: Int -> [Int]
splitrevnumber x =
  if (mod x 10) == x && (div x 10) == 0 then [x]
  else (mod x 10) : splitrevnumber (div x 10)
