module  Arithmetic
( isPrime,
  gcd',
  coprime,
  totientPhi,
  primeFactors,
) where

--31. Determine whether a given integer number is prime.
primes :: (Integral a) =>  [a]
primes = primes' 2
      where primes' x = x: filter (\p -> p `mod` x /= 0) (primes' $ x+1)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = length (filter (\p -> p == n) $ takeWhile (\p -> p <= n) $ primes) == 1

--32. GCD
gcd' :: Int -> Int -> Int
gcd' a b
      | a < b = gcd' b a
      | mod a b == 0 = b
      | otherwise = gcd' b r
          where
            r = mod a b

coprime :: Int -> Int -> Bool
coprime a b = 1 == gcd' a b

totientPhi :: Int -> Int
totientPhi 1 = 1
totientPhi n = length $ filter (\x -> coprime n x) [1 .. n - 1]

primeFactors :: Int -> [Int]
primeFactors n
  | n == 0    = []
  | n == 1    = []
  | isPrime n = [n]
  | otherwise = first : primeFactors(quot n first)
    where first = head $ filter (\p -> mod n p == 0) $ takeWhile (\p -> p <= n) $ primes
