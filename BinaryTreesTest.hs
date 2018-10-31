import Test.HUnit
import BinaryTrees

testConstruct = TestCase (assertEqual "construct binary tree" (Branch 4 (Branch 2 Empty Empty) (Branch 10 Empty Empty)) (construct [4,2,10]))

tests = TestList [TestLabel "construct" testConstruct]
