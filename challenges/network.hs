-- Dada una dirección IPv4 y una máscara de subred
import Text.Regex.Posix

splitNetworkOct :: [Char] -> [[Int]]
splitNetworkOct xs =
  if length xs > 15 then error "IP no valida"
  else 

network :: [Char] -> [Char]
network xs = xs

broadcast :: [Char] -> [Char]
broadcast xs = xs

firsthost :: [Char] -> [Char]
firsthost xs = xs

lasthost :: [Char] -> [Char]
lasthost xs = xs
