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

testDropEmptyList = TestCase(assertBool "should return empty when dropping from empty list" (null (dropEvery [] 10)))
testDropAllElements = TestCase(assertBool "should return empty when dropping every 1 list" (null (dropEvery [1..10] 1)))
testDropAllOdds = TestCase(assertEqual "should return even numbers after dropping odds" [1,3..9] (dropEvery [1..10] 2))

testSplitingForNegativeIndex = TestCase(assertEqual "should split list in two" [[],[1..10]] (split [1..10] (-1)))
testSplitingNotEmptyListForFirstIndex = TestCase(assertEqual "should split list in two" [[1],[2..10]] (split [1..10] 1))
testSplitingNotEmptyListBasedOnMiddleIndex = TestCase(assertEqual "should split list in two" [[1..5], [6..10]] (split [1..10] 5))

testSliceEmptyList = TestCase (assertBool "should split empty list to empty list" (null (slice [] 2 3)))
testSliceSublist = TestCase (assertEqual "should split not empty list" [2..10] (slice [1..12] 2 10))
testSliceSingleSublist = TestCase (assertEqual "should split not empty list" [2] (slice [1..12] 2 2))

testRotatePositiveOffset = TestCase(assertEqual "should rotate with positive offset" "defghabc" (rotate ['a','b','c','d','e','f','g','h'] 3))
testRotateNegativeOffset = TestCase(assertEqual "should rotate with negative offset" "ghabcdef" (rotate ['a','b','c','d','e','f','g','h'] (-2)))

testRemoveAtFromSingleton = TestCase(assertBool "should remove from singleton" (null (removeAt 1 [1])))
testRemoveAtElement = TestCase(assertEqual "should remove first element from list" [2..10] (removeAt 1 [1..10]))
testRemoveAtLastElement = TestCase(assertEqual "should remove last element from list" [1..9] (removeAt 10 [1..10]))

testInsertAtEmptyList = TestCase(assertEqual "should insert at empty array" "a" (insertAt 'a' [] 1))
testInsertAtTheBeginning = TestCase(assertEqual "should insert at the beginning" "abcd" (insertAt 'a' "bcd" 1))
testInsertAtTheEnd = TestCase(assertEqual "should insert at the end" "abcde" (insertAt 'e' "abcd" 5))

testRangeForInvalidRange = TestCase(assertBool "should construct empty range when from > to" (null (range 2 1)))
testRangeForSingleton = TestCase(assertEqual "should construct singleton range" [8] (range 8 8))
testRange = TestCase(assertEqual "should construct from .. to range" [10..20] (range 10 20))

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
                  TestLabel "dropEvery" testDropEmptyList, testDropAllElements, testDropAllOdds,
                  TestLabel "split" testSplitingForNegativeIndex, testSplitingNotEmptyListForFirstIndex, testSplitingNotEmptyListBasedOnMiddleIndex,
                  TestLabel "slice" testSliceEmptyList, testSliceSublist, testSliceSingleSublist,
                  TestLabel "removeAt" testRemoveAtFromSingleton, testRemoveAtElement, testRemoveAtLastElement,
                  TestLabel "rotate" testRotatePositiveOffset, testRotateNegativeOffset,
                  TestLabel "insertAt" testInsertAtEmptyList, testInsertAtTheBeginning, testInsertAtTheEnd,
                  TestLabel "range" testRangeForInvalidRange, testRangeForSingleton, testRange]
