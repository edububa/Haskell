-- challenge 70: reverse a number
reversenumber :: Int -> Int
reversenumber x = f x (ncifras x)
  where
    ncifras = \x -> if div x 10 == mod x 10 then 0 else 1 + ncifras(div x 10)
    f = \x n -> if (n - 1) == 0 then x else (mod x 10) * 10 ^ (n - 1) + f (div x 10) (n - 1)

-- other implementation
reversenumberf :: Int -> Int
reversenumberf x =
  foldr (+) (0) (zipWith (*) (reverse (map (10^) [0..((length (splitrevn x)) - 1)])) (splitrevn x))
  where
    splitrevn = \x -> if (mod x 10) == x && (div x 10) == 0 then [x] else (mod x 10) : splitrevn (div x 10)
