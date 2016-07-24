-- Problem 1
myLast :: [a] -> a
myLast []     = error "no end for empty list"
myLast [x]    = x
myLast (_:xs) = myLast xs

myLast2 :: [a] -> a
myLast2 xs = head (reverse xs)

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
