-- 99 Haskell questions

-- Problem 1: Find the last element of a list.

myLast :: [a] -> a
myLast [] = error "Empty list!"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2: Find the last but one element of a list.

myButLast :: [a] -> a
myButLast [] = error "Empty list!"
myButLast [x] = error "One element list!"
myButLast (x:xs) =
  if length xs == 1 then x
  else myButLast xs

-- Problem 3: Find the K'th element of a list. The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index out of bounds!"
elementAt (x:_) 1 = x
elementAt (x:xs) i = elementAt xs (i - 1)

-- Problem 4: Find the number of elements of a list.

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


-- Problem 5: Reverse a list

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6: Find out whether a list is a palindrome. A palindrome can be read forward or backward

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = xs == myReverse xs

-- Problem 7: Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with
-- its elements (recursively).
