module Perm(perm, tests) where

perm :: [Int] -> [Int] -> Bool
perm l1 l2 = perm_aux (reverse l1) (reverse l2) []

perm_aux :: [Int] -> [Int] -> [Int] -> Bool
perm_aux [] [] [] = True
perm_aux input (i2:output) (i1:stack) | i1 == i2 = perm_aux input output stack
perm_aux (i1:input) output stack = perm_aux input output (i1 : stack)
perm_aux _ _ _ = False

test :: ([Int], [Int], Bool) -> Int
test (input, output, expected)
    | result == expected = 0
    | otherwise = 1
        where
            result = perm input output
tests = putStrLn $ "Test failed: " ++ show count
    where 
        count = sum $ map test test_cases
        test_cases = [([1,2],[2,1],True), ([2,1],[2,1],True), ([2,1],[1,2],True),([1,2],[1,2],True),([1,2,3],[3,2,1],True),([1,2,3],[3,1,2],True),([1,2,3],[2,1,3],True),([1,2,3],[2,3,1],False),([1,2,3],[1,2,3],True),([1,2,3],[1,3,2],True),([2,1,3],[3,2,1],True),([2,1,3],[3,1,2],True),([2,1,3],[2,1,3],True),([2,1,3],[2,3,1],True),([2,1,3],[1,2,3],True),([2,1,3],[1,3,2],False),([3,1,2],[3,2,1],True),([3,1,2],[3,1,2],True),([3,1,2],[2,1,3],True),([3,1,2],[2,3,1],True),([3,1,2],[1,2,3],False),([3,1,2],[1,3,2],True),([2,3,1],[3,2,1],True),([2,3,1],[3,1,2],False),([2,3,1],[2,1,3],True),([2,3,1],[2,3,1],True),([2,3,1],[1,2,3],True),([2,3,1],[1,3,2],True),([3,2,1],[3,2,1],True),([3,2,1],[3,1,2],True),([3,2,1],[2,1,3],False),([3,2,1],[2,3,1],True),([3,2,1],[1,2,3],True),([3,2,1],[1,3,2],True),([1,3,2],[3,2,1],False),([1,3,2],[3,1,2],True),([1,3,2],[2,1,3],True),([1,3,2],[2,3,1],True),([1,3,2],[1,2,3],True),([1,3,2],[1,3,2],True),([1,2,3,4],[1,2,3,4],True),([1,2,3,4],[1,2,4,3],True),([1,2,3,4],[1,4,2,3],True),([1,2,3,4],[1,4,3,2],True),([1,2,3,4],[1,3,4,2],False),([1,2,3,4],[1,3,2,4],True),([1,2,3,4],[4,2,3,1],False),([1,2,3,4],[4,2,1,3],True),([1,2,3,4],[4,1,2,3],True),([1,2,3,4],[4,1,3,2],True),([1,2,3,4],[4,3,1,2],True),([1,2,3,4],[4,3,2,1],True),([1,2,3,4],[2,4,3,1],False),([1,2,3,4],[2,4,1,3],False),([1,2,3,4],[2,1,4,3],True),([1,2,3,4],[2,1,3,4],True),([1,2,3,4],[2,3,1,4],False),([1,2,3,4],[2,3,4,1],False),([1,2,3,4],[3,4,2,1],False),([1,2,3,4],[3,4,1,2],False),([1,2,3,4],[3,1,4,2],False),([1,2,3,4],[3,1,2,4],True),([1,2,3,4],[3,2,1,4],True),([1,2,3,4],[3,2,4,1],False),([],[],True),([1],[],False),([1,2],[],False),([],[1],False),([],[2,1],False),([1],[1],True),([1,2],[1],False),([1],[2,1],False),([1..1000000],[1..1000000], True),([1..1000000],reverse [1..1000000], True)]