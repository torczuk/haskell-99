module  Arithmetic
( isPrime,
  gcd',
  coprime,
) where

--31. Determine whether a given integer number is prime.
primes :: (Integral a) =>   [a]
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
