import Test.HUnit
import Arithmetic

testThat2IsPrime = TestCase(assertBool "2 is prime" (isPrime 2))
testThat199IsPrime = TestCase(assertBool "199 is prime" (isPrime 199))
testThat991IsPrime = TestCase(assertBool "991 is prime" (isPrime 991))
testThat993IsNotPrime = TestCase(assertBool "991 is prime" (not (isPrime 993)))
testGCD1 = TestCase(assertEqual "great common division of 20 18 is 2" 2 (gcd' 20 18))
testGCD2 = TestCase(assertEqual "great common division of prime and non prime is 1" 1 (gcd' 991 91))
testCoPrime = TestCase(assertBool "35 and 64 are coprime" (coprime 35 64))
testAreNotCoPrime = TestCase(assertBool "35 and 15 are not coprime" (not (coprime 35 15)))
testTotient = TestCase(assertEqual "number of coprime with 10 are 4" 4 (totientPhi 10))
testPrimeFactorsOfPrime = TestCase(assertEqual "prime factors of prime should be prime" [991] (primeFactors 991))
testPrimeFactorsOfNotPrime = TestCase(assertEqual "prime factors of not prime" [3, 3, 5, 7] (primeFactors 315))
testPrimeMultiFactorOfPrime = TestCase(assertEqual "prime factors of prime should be prime" [(991, 1)] (primeMultiFactors 991))
testPrimeMultiFactor = TestCase(assertEqual "prime multi factors of not prime" [(3, 2), (5, 1), (7, 1)] (primeMultiFactors 315))
testPrimeRange = TestCase(assertEqual "prime between 10 and 20" [11,13,17,19] (primeR 10 20))

tests = TestList [TestLabel "isPrime" testThat2IsPrime, testThat199IsPrime, testThat991IsPrime, testThat993IsNotPrime,
                  TestLabel "gcd" testGCD1, testGCD2,
                  TestLabel "coprime" testCoPrime, testAreNotCoPrime,
                  TestLabel "totient-phi" testTotient,
                  TestLabel "primeFactors" testPrimeFactorsOfPrime, testPrimeFactorsOfNotPrime,
                  TestLabel "primeMultiFactors" testPrimeMultiFactorOfPrime, testPrimeMultiFactor,
                  TestLabel "primeR" testPrimeRange]
