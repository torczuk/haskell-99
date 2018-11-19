import Test.HUnit
import BinaryTrees

tree2 = Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)
tree4' = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)

testEq = TestCase (assertEqual "the same trees should be the same" tree4 tree4')
testConstruct = TestCase (assertEqual "construct binary tree" (Branch 4 (Branch 2 Empty Empty) (Branch 10 Empty Empty)) (construct [4,2,10]))
testEmptySymetric = TestCase (assertEqual "empty tree is symetric" True (symetric (Branch 1 Empty Empty)))
testBinarySymetric7elements = TestCase (assertEqual "test binary tree symetric [5, 3, 18, 1, 4, 12, 21]" True (testSymetric [5, 3, 18, 1, 4, 12, 21]))
testBinarySymetric5elements = TestCase (assertEqual "test binary tree symetric [3, 2, 5, 7, 1]" True (testSymetric [3, 2, 5, 7, 1]))
testBinarySymetricList = TestCase (assertEqual "test all binary symetric balanced for 5" 2 (length . symCbalTrees $ 5))
testCountLeafs =  TestCase (assertEqual "leafs size in tree4" 2 (countLeaves tree4))
testLeafs =  TestCase (assertEqual "leafs in tree4" [4, 2] (leaves tree4))
testInternals = TestCase (assertEqual "internals in tree4" [1,2] (internals tree4))
testAtLevel = TestCase (assertEqual "internals in tree4" [2,2] (atLevel tree4 2))
testIsCompleteBinaryTree = TestCase (assertEqual "is complete binary tree" True (isCompleteBinaryTree tree2))

tests = TestList [TestLabel "eq" testEq,
                  TestLabel "construct" testConstruct,
                  TestLabel "symetric" testEmptySymetric,
                  TestLabel "symetric" testBinarySymetric7elements, testBinarySymetric5elements,
                  TestLabel "symCbalTrees" testBinarySymetricList,
                  TestLabel "countLeaves" testCountLeafs,
                  TestLabel "leafs" testCountLeafs,
                  TestLabel "internals" testInternals,
                  TestLabel "atLevel" testAtLevel,
                  TestLabel "isCompleteBinaryTree" testIsCompleteBinaryTree]
