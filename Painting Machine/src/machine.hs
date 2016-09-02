import Data.Char
import Control.Monad

indexList :: String -> [(Int, String)]
indexList xs = zip [0..] $ listOfTwo xs
  where
    listOfTwo  = \x -> if length x == 1 then [x] else takeTwo x : listOfTwo (tail x)
    takeTwo    = \(x:xs) -> x : head xs : []

lineStarts :: String -> [Int]
lineStarts xs = if head xs == '#' then 0 : f xs else f xs
  where
    start = \x -> x /= [] && length x > 1 && head x == '.' && head (tail x) == '#'
    f     = \x -> foldr (\x acc -> if start (snd x) then (fst x + 1) : acc else acc) [] $ indexList x

lineEnds :: String -> [Int]
lineEnds xs = if last xs == '#' then g xs ++ [length xs - 1] else g xs
  where
    end = \x -> x /= [] && length x > 1 && head x == '#' && head (tail x) == '.'
    g   = \x -> foldr (\x acc -> if end (snd x) then (fst x) : acc else acc) [] $ indexList x

lineAnalyzer :: String -> [(Int,Int)]
lineAnalyzer xs = zip (lineStarts xs) (lineEnds xs)

instgen :: [(Int, [(Int, Int)])] -> [String]
instgen xs = foldr (\x acc -> (genLines x) ++ acc) [] xs
  where
    genLines  = \(n,xs) -> map (paintline n) xs
    paintline = \x (y,z) -> "PAINT LINE " ++ (show x) ++ " " ++ (show y) ++ " "
                                          ++ (show x) ++ " " ++ (show z)

main :: IO ()
main = do
  input <- getLine
  let n = map read $ words input :: [Int]
      c = head n
      r = head $ tail n
  rs <- sequence $ map (const getLine) [0..(c-1)]
  let result = instgen $  zip [0..] $ map lineAnalyzer rs
  when ((length result) < (r * c)) $ do
    mapM_ putStrLn result
  return ()
