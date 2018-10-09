import Test.HUnit
import Arithmetic

testThat2IsPrime = TestCase(assertBool "2 is prime" (isPrime 2))
testThat199IsPrime = TestCase(assertBool "199 is prime" (isPrime 199))
testThat991IsPrime = TestCase(assertBool "991 is prime" (isPrime 991))
testThat993IsNotPrime = TestCase(assertBool "991 is prime" (not (isPrime 993)))

tests = TestList [TestLabel "isPrime" testThat2IsPrime, testThat199IsPrime, testThat991IsPrime, testThat993IsNotPrime]
