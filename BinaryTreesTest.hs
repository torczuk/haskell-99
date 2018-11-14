import Test.HUnit
import BinaryTrees

testConstruct = TestCase (assertEqual "construct binary tree" (Branch 4 (Branch 2 Empty Empty) (Branch 10 Empty Empty)) (construct [4,2,10]))
testEmptySymetric = TestCase (assertEqual "empty tree is symetric" True (symetric (Branch 1 Empty Empty)))
testBinarySymetric7elements = TestCase (assertEqual "test binary tree symetric [5, 3, 18, 1, 4, 12, 21]" True (testSymetric [5, 3, 18, 1, 4, 12, 21]))
testBinarySymetric5elements = TestCase (assertEqual "test binary tree symetric [3, 2, 5, 7, 1]" True (testSymetric [3, 2, 5, 7, 1]))
testBinarySymetricList = TestCase (assertEqual "test all binary symetric balanced for 5" 2 (length . symCbalTrees $ 5))

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)
testCountLeafs =  TestCase (assertEqual "leafs size in tree4" 2 (countLeaves tree4))
testLeafs =  TestCase (assertEqual "leafs in tree4" [4, 2] (leaves tree4))


tests = TestList [TestLabel "construct" testConstruct,
                  TestLabel "symetric" testEmptySymetric,
                  TestLabel "symetric" testBinarySymetric7elements, testBinarySymetric5elements,
                  TestLabel "symCbalTrees" testBinarySymetricList,
                  TestLabel "countLeaves" testCountLeafs,
                  TestLabel "testLeafs" testCountLeafs]
