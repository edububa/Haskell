-- challenge 80: Factorial
--- recursive version
factorialrec :: Integer -> Integer
factorialrec x = if x == 0 then 1 else x * factorialrec (x - 1)

--- tail recursive version
factorialtailrec :: Integer -> Integer -> Integer
factorialtailrec x y = if x <= 1 then y else factorialtailrec (x - 1) (y * x)

--- nonrecursive version
factorial :: Integer -> Integer
factorial x = foldr (*) (1) [1..x]
