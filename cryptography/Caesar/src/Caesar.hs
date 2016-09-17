import qualified Data.Map as Map
import Data.Maybe as M
import Control.Monad

alphabetC :: Map.Map Char Int
alphabetC = Map.fromList $ zip ['A'..'Z'] [0..]

alphabetI :: Map.Map Int Char
alphabetI = Map.fromList $ zip [0..] ['A'..'Z']

toIntList :: String -> [Int]
toIntList xs = M.mapMaybe (\x -> Map.lookup x alphabetC) xs

toString :: [Int] -> String
toString xs = M.mapMaybe (\x -> Map.lookup x alphabetI) xs

applyKey :: Int -> [Int] -> [Int]
applyKey k xs = map (\x -> mod (k + x) 26) xs

decipher :: Int -> String -> String
decipher k xs = unwords $ map (toString . (applyKey $ -k) . toIntList) $ words xs

cipher :: Int -> String -> String
cipher k xs = unwords $ map (toString . (applyKey k) . toIntList) $ words xs
