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

testInfixlTrue = TestCase(assertEqual "True and not False" True (True `and'` not' False))
testInfixlFalse = TestCase(assertEqual "False or not True" False (False `or'` not' True))

testGrayCode = TestCase(assertEqual "should generate 4 lenght gray code" ["000","001","011","010","110","111","101","100"] (gray 3))

huffmanCode = TestCase(assertEqual "should generate huffman codes" [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
                                                                        (huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]))

tests = TestList [TestLabel "not'" testNotOnTrue, testNotOnFalse,
                  TestLabel "and'" testAndOnTT, testAndOnTF, testAndOnFT, testAndOnFF,
                  TestLabel "or'" testOrOnTT, testOrOnTF, testOrOnFT, testOrOnFF,
                  TestLabel "xor'" testXorOnTT, testXorOnTF, testXorOnFT, testXorOnFF,
                  TestLabel "impl'" testImplOnTT, testImplOnTF, testImplOnFT, testImplOnFF,
                  TestLabel "impl'" testEquOnTT, testEquOnTF, testEquOnFT, testEquOnFF,
                  TestLabel "infix" testInfixlTrue, testInfixlFalse,
                  TestLabel "gray" testGrayCode,
                  TestLabel "huffman" huffmanCode]
