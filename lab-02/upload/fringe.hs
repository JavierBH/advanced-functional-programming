module Fringe(mirror, fibtree_naive) where

import System.IO

data Tree a = E | T a (Tree a) (Tree a) deriving (Show, Read)

-- (a)
mirror :: Tree a -> Tree a
mirror E = E
mirror (T a left right) = T a (mirror right) (mirror left)


-- (b)
fringe_naive :: Tree a -> [a]
fringe_naive E = []
fringe_naive (T a E E) = [a]
fringe_naive (T _ left right) = fringe_naive(left) ++ fringe_naive(right)

-- (c)
fringe :: Tree a -> [a]
fringe t = fringe_aux t []

fringe_aux :: Tree a -> [a] -> [a]
fringe_aux E _ = []
fringe_aux (T a E E) x = a:x
fringe_aux (T a l r) x = (fringe_aux l (fringe_aux r x))
 
--(d)
same_fringe :: Eq a => Tree a -> Tree a -> Bool
same_fringe t1 t2 = (fringe t1) == (fringe t2)

fibtree_naive :: Int -> Tree Int
fibtree_naive n | n <= 1 = T n E E
                | otherwise = T (left + right) (T left l1 l2) (T right r1 r2)
                    where
                        T left l1 l2 = fibtree_naive (n-1)
                        T right r1 r2 = fibtree_naive (n-2)


fibtree :: Int -> Tree Int
fibtree n | n <= 1 = T n E E 
          | otherwise = fibtree_aux (n-2) ((T 1 E E, T 0 E E))

fibtree_aux :: Int -> (Tree Int, Tree Int)  -> Tree Int
fibtree_aux n (T v1 t1 t2, T v2 t3 t4)
    | n < 0 = T v1 t1 t2
    | otherwise = fibtree_aux (n - 1) ((T (v1+v2) (T v1 t1 t2) (T v2 t3 t4), T v1 t1 t2))


main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    putStr "Give n: "
    s <- getLine
    let n = read s
    let t1 = fibtree n
    let t2 = mirror t1
    putStr (if same_fringe t1 t2 then "yes\n" else "no\n")
    let t3 = fibtree (n+1)
    putStr (if same_fringe t1 t3 then "yes\n" else "no\n")