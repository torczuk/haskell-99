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

testEncodeModifiedOnNonEmpty = TestCase (assertEqual "encodeModified ['a' 'a' 'b'] == [Multiple 2 'a', Single b]" [Multiple 'a' 2, Single 'b'] (encodeModified ['a', 'a', 'b']))

testDecodeModified = TestCase (assertEqual "should decodeModified for not empty list containing Single and Multiple" ['a', 'a', 'b'] (decodeModified [Multiple 'a' 2, Single 'b']))

testEncodeDirect = TestCase(assertEqual "should encode direct non empty list" [Multiple 'a' 2, Single 'b', Single 'a'] (encodeDirect ['a', 'a', 'b', 'a']))

testDupliEmptyList = TestCase(assertBool "dupli [] = []" (null . dupli $ []))
testDupliNonEmptyList = TestCase(assertEqual "dupli [a a b c] == [a a a a b b c c]" ['a', 'a', 'a', 'a', 'b', 'b', 'c', 'c'] (dupli ['a', 'a', 'b', 'c']))

testRepliEmptyList = TestCase(assertBool "repli [] n = []" (null (repli [] 100)))
testReplZero = TestCase(assertBool "repli [1 1 2] 0 = []" (null (repli [1..100] 0)))
testRepliOnce = TestCase(assertEqual "repli [1 1 2] 1 = [1 1 2]" [1, 1, 2] (repli [1, 1, 2] 1))
testRepliNonEmpty = TestCase(assertEqual "repli [1 1 2] 3 = [1 1 1 1 1 1 2 2 2]" [1, 1, 1, 1, 1, 1, 2, 2, 2] (repli [1, 1, 2] 3))

testDropFromSingleton = TestCase(assertBool "should drop from singleton" (null (drop'  [1] 1)))
testDropFirstElement = TestCase(assertEqual "should drop first element from list" [2..10] (drop' [1..10] 1))
testDropLastElement = TestCase(assertEqual "should drop last element from list" [1..9] (drop' [1..10] 10))

testSplitingForNegativeIndex = TestCase(assertEqual "should split list in two" [[],[1..10]] (split [1..10] (-1)))
testSplitingNotEmptyListForFirstIndex = TestCase(assertEqual "should split list in two" [[1],[2..10]] (split [1..10] 1))
testSplitingNotEmptyListBasedOnMiddleIndex = TestCase(assertEqual "should split list in two" [[1..5], [6..10]] (split [1..10] 5))

testSliceEmptyList = TestCase (assertBool "should split empty list to empty list" (null (slice [] 2 3)))
testSliceNonEmptyList = TestCase (assertEqual "should split not empty list" [2..10] (slice [1..12] 2 10))

tests = TestList [TestLabel "myLast suites" testMyLastSingleton, testMyLastNotSingleton,
                  TestLabel "butLast suites" testButLastTwoElements, testButLastMoveThanTwoElements,
                  TestLabel "elementAt suites" testElementAtFirstElement, testElementAtLastElement,
                  TestLabel "myLength suites" testMyLengthOnEmptyList, testMyLengthOnNonEmptyList,
                  TestLabel "myReverse suites" testMyReverseOnEmptyList, testMyReverseOnSingleton, testMyReverseOnAscList,
                  TestLabel "compress suites" testCompressOnEmpty, testCompressOnNonEmpty, testCompressTheSameElements,
                  TestLabel "palindrome" testIsPalindromeForEmpty, testIsPalindromeFoSinleton, testIsPalindromeForPalindrome, testIsPalindromeForNonPalindrome,
                  TestLabel "pack suites" testPackOnSingleton, testPackOnNonMultiple,
                  TestLabel "encode suites" testEncodeOnNonMultiple,
                  TestLabel "dupli suites" testDupliEmptyList, testDupliNonEmptyList,
                  TestLabel "repli suites" testRepliEmptyList, testReplZero, testRepliOnce, testRepliNonEmpty,
                  TestLabel "encodeModified suites" testEncodeModifiedOnNonEmpty,
                  TestLabel "decodeModified" testDecodeModified,
                  TestLabel "encodeDirect" testEncodeDirect,
                  TestLabel "drop" testDropFromSingleton, testDropFirstElement, testDropLastElement,
                  TestLabel "split" testSplitingForNegativeIndex, testSplitingNotEmptyListForFirstIndex, testSplitingNotEmptyListBasedOnMiddleIndex,
                  TestLabel "slice" testSliceEmptyList, testSliceNonEmptyList]
