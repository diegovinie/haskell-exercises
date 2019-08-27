import Data.List

isPrime :: Integral a => a -> Bool
isPrime n
  | n == 2                      = True
  | (n < 2) || (rem n 2 == 0)   = False
  | otherwise                   = recPrime n 3

recPrime :: Integral a => a -> a -> Bool
recPrime n k
  | n < k * k           = True
  | (rem n k) == 0      = False
  | otherwise           = recPrime n (k + 2)

nextPrime :: Int -> Int
nextPrime n
  | isPrime (n + 1) = n + 1
  | otherwise = nextPrime (n + 1)

search :: Int -> [Int] -> Int
search max nums@(n:ns)
  | (product nums) <= max  = search max ((nextPrime n):nums)
  | otherwise             = length ns

primeCount :: Int -> Int
primeCount 1 = 0
primeCount 2 = 1
primeCount n = search n [2]
