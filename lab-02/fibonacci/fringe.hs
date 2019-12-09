module Fringe(mirror, fibtree_naive) where

data Tree a = E | T a (Tree a) (Tree a) deriving (Show, Read)

-- (a)
mirror :: Tree a -> Tree a
mirror E = E
mirror (T a left right) = T a (mirror right) (mirror left)


-- (b)
fringe_naive :: Tree a -> [a]
fringe_naive (T a E E) = [a]
fringe_naive (T _ left right) = fringe_naive(left) ++ fringe_naive(right)
-- Complexity of this solution is O(n)

-- (c)
fringe :: Tree a -> [a]
fringe (T a E E) = [a]
fringe (T _ left right) = fringe_naive(left) ++ fringe_naive(right)


--(d)
same_fringe :: Eq a => Tree a -> Tree a -> Bool
same_fringe t1 t2 = (fringe t1) == (fringe t2)
-- Complexity of this solution is O(2*n)

fibtree_naive :: Int -> Tree Int
fibtree_naive n | n <= 1 = T n E E
                | otherwise = T (left + right) (T left l1 l2) (T right r1 r2)
                    where
                        T left l1 l2 = fibtree_naive (n-1)
                        T right r1 r2 = fibtree_naive (n-2)

