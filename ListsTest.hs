import Test.HUnit
import Data.List
import Lists

testMyLastSingleton = TestCase (assertEqual "myLast [10] == x" 2 (myLast [2]))
testMyLastNotSingleton = TestCase (assertEqual "myLast [1..x] == x" 100 (myLast [1..100]))

testButLastTwoElements = TestCase (assertEqual "butLast [1 2] == 2" 1 (butLast [1, 2]))
testButLastMoveThanTwoElements = TestCase (assertEqual "butLast [1..x] == x - 1" 98 (butLast [1..99]))

testElementAtFirstElement = TestCase (assertEqual "elementAt [x] 1 == x" 'a' (elementAt "abcd" 1))
testElementAtLastElement = TestCase (assertEqual "elementAt [1..100] 100 == 100" 100 (elementAt [1..100] 100))

testMyLengthOnEmptyList = TestCase (assertEqual "myLength [] == 0" 0 (myLength []))
testMyLengthOnNonEmptyList = TestCase (assertEqual "myLength [1..100] == 100" 100 (myLength [1..100]))

testMyReverseOnEmptyList = TestCase (assertBool "myReverse [] == []" (null . myReverse $ []))
testMyReverseOnSingleton = TestCase (assertEqual "myReverse [x] == [x]" "a" (myReverse "a"))
testMyReverseOnAscList = TestCase (assertEqual "myReverse [1..x] == [x..1]" "lleksah" (myReverse "haskell"))

testIsPalindromeForEmpty = TestCase (assertEqual "isPalindrome [] == [x..1]" "lleksah" (myReverse "haskell"))
testIsPalindromeFoSinleton = TestCase (assertEqual "isPalindrome [1..x] == [x..1]" "lleksah" (myReverse "haskell"))
testIsPalindromeForPalindrome = TestCase (assertEqual "isPalindrome [1..x] == [x..1]" "lleksah" (myReverse "haskell"))
testIsPalindromeForNonPalindrome = TestCase (assertEqual "isPalindrome [1..x] == [x..1]" "lleksah" (myReverse "haskell"))

testCompressOnEmpty = TestCase (assertBool "compress [] == []" (null . myReverse $ []))
testCompressOnNonEmpty = TestCase (assertEqual "compress [a b b a a b b] == [a b a b]" ['a', 'b', 'a', 'b'] (compress ['a', 'b', 'b', 'a' ,'a', 'b']))
testCompressTheSameElements = TestCase (assertEqual "compress [a*] == [a]" ['a'] (compress ['a', 'a', 'a', 'a' ,'a', 'a']))

testPackOnSingleton = TestCase(assertEqual "pack ['a'] == ['a']" ["a"] (pack ['a']))
testPackOnNonMultiple = TestCase(assertEqual "pack ..." ["aaaa","b","cc","aa","d","eeee"] (pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']))

testEncodeOnNonMultiple = TestCase(assertEqual "encode [aaa bb cc a] == [(3, a), (2, b) (2, c) (1, c)]" [(3, 'a'), (2, 'b'), (2, 'c'), (1, 'a')] (encode ["aaa", "bb", "cc", "a"]))

testDupliEmptyList = TestCase(assertBool "dupli [] = []" (null . dupli $ []))
testEncodeNonEmptyList = TestCase(assertEqual "encode [a a b c] == [a a a a b b c c]" ['a', 'a', 'a', 'a', 'b', 'b', 'c', 'c'] (dupli ['a', 'a', 'b', 'c']))

tests = TestList [TestLabel "myLast suites" testMyLastSingleton, testMyLastNotSingleton,
                  TestLabel "butLast suites" testButLastTwoElements, testButLastMoveThanTwoElements,
                  TestLabel "elementAt suites" testElementAtFirstElement, testElementAtLastElement,
                  TestLabel "myLength suites" testMyLengthOnEmptyList, testMyLengthOnNonEmptyList,
                  TestLabel "myReverse suites" testMyReverseOnEmptyList, testMyReverseOnSingleton, testMyReverseOnAscList,
                  TestLabel "compress suites" testCompressOnEmpty, testCompressOnNonEmpty, testCompressTheSameElements,
                  TestLabel "pack suites" testPackOnSingleton, testPackOnNonMultiple,
                  TestLabel "encode suites" testEncodeOnNonMultiple, testEncodeNonEmptyList]
