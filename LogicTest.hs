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

testImplOnTT = TestCase(assertEqual "impl True True" True (impl' True True))
testImplOnTF = TestCase(assertEqual "impl True False" False (impl' True False))
testImplOnFT = TestCase(assertEqual "impl False True" True (impl' False True))
testImplOnFF = TestCase(assertEqual "impl False False" True (impl' False False))

testEquOnTT = TestCase(assertEqual "equ True True" True (equ' True True))
testEquOnTF = TestCase(assertEqual "equ True False" False (equ' True False))
testEquOnFT = TestCase(assertEqual "equ False True" False (equ' False True))
testEquOnFF = TestCase(assertEqual "equ False False" True (equ' False False))

tests = TestList [TestLabel "not'" testNotOnTrue, testNotOnFalse,
                  TestLabel "and'" testAndOnTT, testAndOnTF, testAndOnFT, testAndOnFF,
                  TestLabel "or'" testOrOnTT, testOrOnTF, testOrOnFT, testOrOnFF,
                  TestLabel "xor'" testXorOnTT, testXorOnTF, testXorOnFT, testXorOnFF,
                  TestLabel "impl'" testImplOnTT, testImplOnTF, testImplOnFT, testImplOnFF,
                  TestLabel "impl'" testEquOnTT, testEquOnTF, testEquOnFT, testEquOnFF]
