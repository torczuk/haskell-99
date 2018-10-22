import Test.HUnit
import Logic

testNotOnTrue = TestCase(assertEqual "not True" False (not' True))
testNotOnFalse = TestCase(assertEqual "not False" True (not' False))

testAndOnTT = TestCase(assertEqual "and True True" True (and' True True))
testAndOnTF = TestCase(assertEqual "and True False" False (and' True False))
testAndOnFT = TestCase(assertEqual "and False True" False (and' False True))
testAndOnFF = TestCase(assertEqual "and False False" False (and' False False))

testOrOnTT = TestCase(assertEqual "or True True" True (or' True True))
testOrOnTF = TestCase(assertEqual "or True False" True (or' True False))
testOrOnFT = TestCase(assertEqual "or False True" True (or' False True))
testOrOnFF = TestCase(assertEqual "or False False" False (or' False False))

testXorOnTT = TestCase(assertEqual "xor True True" False (xor' True True))
testXorOnTF = TestCase(assertEqual "xor True False" True (xor' True False))
testXorOnFT = TestCase(assertEqual "xor False True" True (xor' False True))
testXorOnFF = TestCase(assertEqual "xor False False" False (xor' False False))

tests = TestList [TestLabel "not'" testNotOnTrue, testNotOnFalse,
                  TestLabel "and'" testAndOnTT, testAndOnTF, testAndOnFT, testAndOnFF,
                  TestLabel "or'" testOrOnTT, testOrOnTF, testOrOnFT, testOrOnFF,
                  TestLabel "xor'" testOrOnTT, testOrOnTF, testOrOnFT, testOrOnFF]
