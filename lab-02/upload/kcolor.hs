module Graph(empty, neighbors) where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Foldable

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)


check_coloring :: Eq a => Graph a -> [(a, Char)] -> Maybe [(a, Char)]
check_coloring g colors | is_valid = Just colors
                        | otherwise = Nothing
    where
        (Graph v _) = g
        neighbors_colors n = [color | (_,color) <- (filter (\(x,_) -> elem x (neighbors g n)) colors)]
        is_valid = (length $ [color | (n,color) <- colors, notElem color (neighbors_colors n)]) == length v


kcolor :: Eq a => [(a, [a])] -> Int -> Maybe [(a, Char)]
kcolor input_graph n = asum [check_coloring g c | c <- combinations]
    where
        combinations = [(zip a x) | a <- permutations $ v, x <- mapM color_list v, head x <= head (tail x)]
        Graph v e = g
        g = parse_graph empty input_graph
        color_list = const ['a'..chr $ (97+n-1)]


parse_graph :: Eq a => Graph a -> [(a, [a])] -> Graph a
parse_graph g [] = g
parse_graph g ((v,[]):input_graph) = parse_graph (addVertex g v) input_graph
parse_graph g ((v,e1:e):input_graph) = parse_graph (addEdge (addVertex g v) (v, e1)) ((v,e):input_graph)

{- Creates a empty graph: empty list of vertices and empty list of edges -}
empty :: Graph a
empty = Graph [] []

{- Add a vertex in the graph -}
addVertex :: Eq a => Graph a -> a -> Graph a
addVertex (Graph v e) new_vertex | notElem new_vertex v = Graph (v ++ [new_vertex]) e
addVertex (Graph v e) _ = Graph v e

{- Add a edge in the graph -}
addEdge :: Eq a => Graph a -> (a,a) -> Graph a
addEdge (Graph v e) (a,b) | a /= b && elem a v && elem b v && notElem (a,b) e && notElem (b,a) e = Graph v (e ++ [(a,b)])
addEdge (Graph v e) _ = Graph v e


{- Creates a list of the first-level neighbors given a vertex -}
neighbors :: Eq a => Graph a -> a -> [a]
neighbors (Graph _ []) _ = []
neighbors (Graph v ((v1,v2):e)) vertex | v1 == vertex = v2 : neighbors (Graph v e) vertex
neighbors (Graph v ((v1,v2):e)) vertex | v2 == vertex = v1 : neighbors (Graph v e) vertex
neighbors (Graph v (_:e)) vertex = neighbors (Graph v e) vertex


test_creation x = addVertex empty x == Graph [x] []
test_add_multiple_vertices x y z
    | x /= y && x /= z = addVertex (addVertex (addVertex empty x) y) z == (Graph [x, y, z] [])
    | x == y && x /= z = addVertex (addVertex (addVertex empty x) y) z == (Graph [x, z] [])
    | x == z && x /= y = addVertex (addVertex (addVertex empty x) y) z == (Graph [x, y] [])
    | y == z && x /= y = addVertex (addVertex (addVertex empty x) y) z == (Graph [x, y] [])
    | otherwise = addVertex (addVertex (addVertex empty x) y) z == (Graph [x] [])
test_repeated_vertex x = addVertex (addVertex empty x) x == (Graph [x] [])
test_edge x y | x /= y = (addEdge (addVertex (addVertex empty x) y) (x, y)) == (Graph [x, y] [(x, y)])
              | otherwise = (addEdge (addVertex (addVertex empty x) y) (x, y)) == (Graph [x] [])
test_repeated_edge x y | x /= y = (addEdge (addEdge (addVertex (addVertex empty x) y) (x,y)) (x,y)) == (Graph [x,y] [(x,y)])
              | otherwise = (addEdge (addVertex (addVertex empty x) y) (x, y)) == (Graph [x] [])
test_neighbors x y z
                    | x /= y && x /= z = (neighbors (addEdge (addEdge (addVertex (addVertex (addVertex empty x) y) z) (x,y)) (x,z)) x) == [y,z] 
                    | x /= y && x == z = (neighbors (addEdge (addEdge (addVertex (addVertex (addVertex empty x) y) z) (x,y)) (x,z)) x) == [y]
                    | x == y && x == z = (neighbors (addEdge (addEdge (addVertex (addVertex (addVertex empty x) y) z) (x,y)) (x,z)) x) == []
                    | otherwise = (neighbors (addVertex (addVertex empty x) y) x) == []
test_neighbors1 x y z = (neighbors (addVertex (addVertex (addVertex empty x) y) z) x) == []
test_neighbors2 x y z | x /= y && x /= z = (neighbors (addEdge (addEdge (addEdge (addVertex (addVertex (addVertex empty x) y) z) (x,y)) (x,z)) (z,y)) x) == [y,z]           
                      | x /= y && x == z = (neighbors (addEdge (addEdge (addEdge (addVertex (addVertex (addVertex empty x) y) z) (x,y)) (x,z)) (z,y)) x) == [y]
                      | otherwise = (neighbors (addEdge (addEdge (addEdge (addVertex (addVertex (addVertex empty x) y) z) (x,y)) (x,z)) (z,y)) x) == []
test_neighbors3 x y z | y /= x && y /= z = (neighbors (addEdge (addEdge (addEdge (addVertex (addVertex (addVertex empty x) y) z) (x,y)) (x,z)) (z,y)) y) == [y,z]           
                      | x /= y && y == z = (neighbors (addEdge (addEdge (addEdge (addVertex (addVertex (addVertex empty x) y) z) (x,y)) (x,z)) (z,y)) y) == [y]
                      | otherwise = (neighbors (addEdge (addEdge (addEdge (addVertex (addVertex (addVertex empty x) y) z) (x,y)) (x,z)) (z,y)) y) == []
test (x, y, z) = (test_creation x) && (test_add_multiple_vertices x y z) && (test_repeated_vertex x)  && (test_edge x y) && (test_repeated_edge x y) 
    && (test_neighbors x y z) && (test_neighbors1 x y z) && (test_neighbors2 x y z) && (test_neighbors3 x y z) 