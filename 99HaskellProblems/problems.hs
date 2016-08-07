-- Problem 1
myLast :: [a] -> a
myLast []     = error "no end for empty list"
myLast [x]    = x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' xs = head (reverse xs)

-- Problem 2
myButLast :: [a] -> a
myButLast xs =
  if length xs <= 1 then error "list with less than 2 elements"
  else head (tail (reverse xs))

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt xs 1 = head xs
elementAt xs y =
  if length xs < y then error "Index out of bounds"
  else elementAt (tail xs) (y - 1)

-- Problem 4
myLength :: [a] -> Int
myLength []     = 0
myLength (x:xs) = 1 + myLength(xs)

myLength' :: [a] -> Int
myLength' xs = foldr (const (+1)) 0 xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

-- Problem 7


-- Problem 8

-- Problem 9

-- Problem 10

