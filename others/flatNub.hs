
member y (x:xs) 
  | xs == [] = False
  | y == x = True
  | otherwise = member y xs

flatNub (x:xs)  = [y ++ x] 
