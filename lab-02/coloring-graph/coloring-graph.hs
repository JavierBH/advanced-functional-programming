module Graph(empty, neighbors) where

import Test.QuickCheck
import Data.Char
import Data.List

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)



class Monad m => MonadPlus m where  
    mzero :: m a  
    mplus :: m a -> m a -> m a  

instance MonadPlus [] where  
    mzero = []  
    mplus = (++) 

guard :: (MonadPlus m) => Bool -> m ()  
guard True = return ()  
guard False = mzero  


get_colors_of :: Eq a => a -> [(a, Char)] -> Graph a -> [Char]
get_colors_of v c g = colors
    where
        neighbor_list = neighbors g v
        colors = [color | (_,color) <- (filter (\(x,_) -> elem x neighbor_list) c)]

        {-
colors = [(1,'a'),(2,'b'),(3,'c')]
g = (Graph [1,2,3] [(1,2),(1,3)])
        -}
valid_coloring :: Eq a => Graph a -> [(a, Char)] -> [Bool]
valid_coloring g colors = do
    (n,c) <- colors
    guard (notElem c (get_colors_of n colors g))
    return True


check_coloring :: Eq a => Graph a -> [[(a, Char)]] -> Maybe [(a, Char)]
check_coloring g [] = Nothing
check_coloring g (c:colors) | nvertexes == valid = Just c
                            | otherwise = check_coloring g colors
    where
        Graph v _ = g
        nvertexes = length v
        valid = length $ valid_coloring g c


kcolor :: Eq a => [(a, [a])] -> Int -> [Maybe [(a, Char)]]
kcolor input_graph n = do
    color <- [(zip a [chr color | color <- x]) | a <- permutations $ v, x <- [x| x <- mapM (const [97..(97+n-1)]) v], head x < head (tail x)]
    guard(check_coloring g colors)
    return color
    where
        Graph v e = g
        colors = [(zip a [chr color | color <- x]) | a <- permutations $ v, x <- [x| x <- mapM (const [97..(97+n-1)]) v], head x < head (tail x) ]
        g = parse_graph empty input_graph



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