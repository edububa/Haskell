-- some Examples of Haskell Functions

doubleMe :: Num a => a -> a
doubleMe x = x + x
 
doubleUs :: Num a => a -> a -> a
doubleUs x y = x*2 + y*2

add :: Num a => (a, a) -> a
add (x,y) = x+y

add' :: Num a => a -> a -> a
add' x y = x + y

zeroto :: (Enum t, Num t) => t -> [t]
zeroto n =  [0..n]

mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

--------------------------------------------------------------

-- 3. Defining Functions Homework 

-- Exercise 1
                
halve :: [a] -> ([a], [a])
halve xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
           where n = length xs

-- Some Other implementation for the halve function
--halve xs = (take n xs, drop n xs)
--  where n = length xs `div` 2
--halve xs = splitAt (div (length xs) 2) xs
--halve xs = splitAt (length xs `div` 2) xs

