module Graph(empty, neighbors) where

import Test.QuickCheck
import Data.Char

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)


kcolor :: Eq a => [(a, [a])] -> Int -> Maybe [(a, Char)]
kcolor input_graph n | input_graph == [] = Just []
                     | otherwise = kcolor_alg (Graph v e) [(v1, 'a')] n
        where
            (Graph (v1:v) e) = parse_graph (Graph [] []) input_graph

kcolor_alg :: Eq a => Graph a -> [(a, Char)] -> Int -> Maybe [(a, Char)]
kcolor_alg (Graph v e) colors n | v == [] = Just colors
                                | ncolors <= n = kcolor_alg (Graph (tail v) e) newColors n
                                | otherwise = Just colors
        where
            v1 = head v
            ncolors | colors /= [] = (ord $ maximum $ [c | (_, c) <- colors]) - (ord 'a') + 1
                    | otherwise = ord 'a'
            neighbors_list = neighbors (Graph v e) v1
            color_list = [ord c | (v, c) <- colors, elem v neighbors_list]
            max | color_list /= [] = maximum color_list
                | otherwise = ord 'a'
            missing = [c | c <- [ord 'a'..max], notElem c color_list]
            newColors | length missing /= 0 = colors ++ [(v1, chr $ head missing)]
                      | otherwise = colors ++ [(v1,chr $ max + 1)]

parse_graph :: Eq a => Graph a -> [(a, [a])] -> Graph a
parse_graph g [] = g
parse_graph g ((_,[]):input_graph) = parse_graph g input_graph
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

vertices :: Eq a => Graph a -> [a]
vertices (Graph v _) = v

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
test (x, y, z) = (test_creation x) && (test_add_multiple_vertices x y z) && (test_repeated_vertex x)  && (test_edge x y) && (test_repeated_edge x y)