-- challenge 80: Factorial
--- recursive version
factorialrec :: Int -> Int
factorialrec x = if x == 0 then 1 else x * factorialrec (x - 1)

--- tail recursive version
factorialtailrec :: Int -> Int -> Int
factorialtailrec x y = if x <= 1 then y else factorialtailrec (x - 1) (y * x)

--- nonrecursive version
factorial :: Int -> Int
factorial x = foldr (*) (1) [1..x]
