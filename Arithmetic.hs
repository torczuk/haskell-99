module  Arithmetic
( isPrime,
  gcd',
  coprime,
  totientPhi,
  primeFactors,
  primeMultiFactors,
  primeR,
  phi,
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

-- 33. Determine whether two positive integer numbers are coprime
coprime :: Int -> Int -> Bool
coprime a b = 1 == gcd' a b

--34.  Calculate Euler's totient function phi(m).
totientPhi :: Int -> Int
totientPhi 1 = 1
totientPhi n = length $ filter (\x -> coprime n x) [1 .. n - 1]

-- 35.  Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
primeFactors :: Int -> [Int]
primeFactors n
  | n == 0    = []
  | n == 1    = []
  | isPrime n = [n]
  | otherwise = first : primeFactors(quot n first)
    where first = head $ filter (\p -> mod n p == 0) $ takeWhile (\p -> p <= n) $ primes

-- 36.  Determine the prime factors of a given positive integer.
primeMultiFactors :: Int -> [(Int, Int)]
primeMultiFactors n = foldr agg []  (primeFactors n)
                where
                  agg e [] = [(e, 1)]
                  agg e (x:xs) = if (fst x) == e then (e, (snd x) + 1)  :xs else (e, 1) :x :xs

-- 37. Calculate Euler's totient function phi(m) (improved).
phi:: Int -> Int
phi n = foldl formula 1 $ primeMultiFactors n
        where
          formula result (prime, multi) = result * (prime - 1) * prime ^ (multi - 1)

-- 39. A list of prime numbers. 
primeR :: Integral a => a -> a -> [a]
primeR up down = takeWhile (\p -> p <= down) $ dropWhile (\p -> p <= up) $ primes
