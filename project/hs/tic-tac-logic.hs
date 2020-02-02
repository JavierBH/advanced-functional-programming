-- Maximo Garcia Martinez & Koen Timmermans
-- Solver for Takuzu puzzles. See also the accompanying report.

--  Imports  --
import Data.List


--  (Type) definitions  --
data Cell = X | O | U deriving (Eq, Show)
type Row = [Cell]
type Grid = [Row]
type Mask = Grid
data Msg = Subsumed | AtFixpt | Unknown | Failed deriving (Show, Eq)
type Propagator = Grid -> (Grid, Msg)
type Masker = Grid -> Cell -> Mask


--  Auxiliary functions  --
--count x xs  returns the amount of occurences of x in xs.
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

--These functions combine cells/rows/masks by (element-wise) taking the left-most argument that is not U.
combinecell :: Cell -> Cell -> Cell
combinecell X _ = X
combinecell O _ = O
combinecell U X = X
combinecell U O = O
combinecell U U = U

combinecell3 :: Cell -> Cell -> Cell -> Cell
combinecell3 X _ _ = X
combinecell3 O _ _ = O
combinecell3 U a b = combinecell a b

combinerows :: Row -> Row -> Row
combinerows = zipWith combinecell

combinerows3 :: Row -> Row -> Row -> Row
combinerows3 = zipWith3 combinecell3

combinemask :: Mask -> Mask -> Mask
combinemask = zipWith combinerows

--These functions return true iff two cells/rows/masks do not have opposite elements in the same position.
cellsok :: Cell -> Cell -> Bool
cellsok O X = False
cellsok X O = False
cellsok _ _ = True

rowsok :: Row -> Row -> Bool
rowsok r l = and (zipWith cellsok r l)

masksok :: Mask -> Mask -> Bool
masksok m n = and (zipWith rowsok m n)

--allunknownrow row  returns true if the row only contains Us.
allunknownrow :: Row -> Bool
allunknownrow = all ((==) U)

--allunknown mask  returns true if the mask only contains Us.
allunknown :: Mask -> Bool
allunknown = and . map allunknownrow

--solved grid  returns true if the grid does not contain any Us.
--Note: it does not check whether the grid is correct.
solved :: Grid -> Bool
solved grid = and (map (not . elem U) grid)

--apply2 applies two masks to a grid.
apply2 :: Grid -> Mask -> Mask -> Grid
apply2 = zipWith3 combinerows3

--cnot reverses the roles of X and O.
cnot :: Cell -> Cell
cnot O = X
cnot X = O


--  Correctness checks  --
--countcorrect row  returns true if at most half of row consists of X and at most half of O.
countcorrect :: Row -> Bool
countcorrect row = let l = div (length row) 2 in
  count X row <= l && count O row <= l

--noduplrows grid  returns true if grid does not contain duplicate rows, ignoring Us.
noduplrows :: Grid -> Bool
noduplrows grid = and [ x /= y | (x:xs) <- tails grid, y <- xs, not (elem U x), not (elem U y)]

--notriples row  returns true if there are no three consecutive Xs or Os in row.
notriples :: Row -> Bool
notriples row = case row of
  (X:X:X:_) -> False
  (O:O:O:_) -> False
  (_:xs) -> notriples xs
  [] -> True

--correct grid  returns true if the grid is correctly filled, ignoring Us
correct :: Grid -> Bool
correct grid = let grid' = transpose grid in
  all notriples grid' && all notriples grid
  && all countcorrect grid' && all countcorrect grid
  && noduplrows grid' && noduplrows grid



--  Basic technique 1  --
--check1 is a helper function for basic1row.
check1 :: Cell -> Row -> Row
check1 t [a,b] = [U,U]
check1 t (a:b:c:xs)
  | a == U && b == cnot t && c == cnot t  = t : check1 t (b:c:xs)
check1 t (x:xs) = U : check1 t xs

--basic1row b row  returns a row with bs where they should be added according to basic technique 1.
basic1row :: Cell -> Row -> Row
basic1row b row = combinerows (check1 b row) (reverse $ check1 b $ reverse row)

--basic1mask  creates the masker for basic technique 1.
basic1mask :: Masker
basic1mask grid b = combinemask (map (basic1row b) grid) (transpose $ map (basic1row b) $ transpose grid)


--  Basic technique 2  --
--check2 is a helper function for basic2row
check2 :: Cell -> Row -> Row
check2 t [a,b] = [U]
check2 t (a:b:c:xs)
  | a == cnot t && b == U && c == cnot t  = t : check2 t (b:c:xs)
check2 t (x:xs) = U : check2 t xs

--basic2row b row  returns a row with bs where they should be added according to basic technique 2.
basic2row :: Cell -> Row -> Row
basic2row b row = U : check2 b row

--basic1mask  creates the masker for basic technique 2.
basic2mask :: Masker
basic2mask grid b = combinemask (map (basic2row b) grid) (transpose $ map (basic2row b) $ transpose grid)

--  Basic technique 3  --
-- Note: This propagator is not used in the final product because there is one combined propagator for basic technique 3 and 5 and advanced technique 2 later.
--oneelt b n  create a list of all lists of n elements consisting of one b and n-1 (cnot b)s.
oneelt :: Cell -> Int -> [Row]
oneelt b 0 = []
oneelt b n = (b : (replicate (n-1) (cnot b))) : [ cnot b : row | row <- oneelt b (n-1) ]

--basic3try b row  returns a row with bs where they should be added according to basic technique 3.
--Note: This funcion should only be called if row lacks only one cell of type (cnot b).
basic3try :: Cell -> Row -> Row
basic3try b row = [ if x == U && not (notriples (combinerows row try)) then b else U | (x,try) <- zip row (oneelt (cnot b) (length row)) ]

--basic3row b row  returns a row with bs where they should be added according to basic technique 3.
basic3row :: Cell -> Row -> Row
basic3row b row =
  case succ (count (cnot b) row) == div (length row) 2 of
    False -> replicate (length row) U
    True -> basic3try b row

--basic3mask  creates the masker for technique 3.
basic3mask :: Masker
basic3mask grid b = combinemask (map (basic3row b) grid) (transpose $ map (basic3row b) $ transpose grid)

--  Basic technique 4  --
--basic4row b row  returns a row with bs where they should be added according to basic technique 4.
basic4row :: Cell -> Row -> Row
basic4row b row = 
  case (count (cnot b) row == div (length row) 2) of
    True -> [ if x == U then b else U | x <- row]
    False -> replicate (length row) U

--basic4mask  creates the masker for basic technique 4.
basic4mask :: Masker
basic4mask grid b = combinemask (map (basic4row b) grid) (transpose $ map (basic4row b) $ transpose grid)


--  Basic technique 5  --
--Basic technique 5 is the special case of Advanced technique 2 where only one X and one O are missing.

--  Advanced technique 2  --
-- Note: This propagator is not used in the final product because there is one combined propagator for basic technique 3 and 5 and advanced technique 2 later.
--adv2try b row  returns a row with bs where they should be added according to advanced technique 2.
--Note: This funcion should only be called if row lacks only one cell of type (cnot b).
adv2try :: Cell -> Grid -> Row -> Row
adv2try b grid row = [ if x == U && elem (combinerows row try) grid then b else U | (x,try) <- zip row (oneelt (cnot b) (length row)) ]

--adv2row b row  returns a row with bs where they should be added according to advanced technique 2.
adv2row :: Cell -> Grid -> Row -> Row
adv2row b grid row = 
  case succ (count (cnot b) row) == div (length row) 2 of
    False -> replicate (length row) U
    True -> adv2try b grid row

--adv2mask  creates the masker for advanced technique 2.
adv2mask :: Masker
adv2mask grid b = let grid' = transpose grid in 
  combinemask (map (adv2row b grid) grid) (map (adv2row b grid') grid')

--  Basic 3 + advanced 2 (+ basic 5)  --
-- These techniques are combined into one propagator. (ct stands for combined techniques)
-- Basic 3 and advanced 2 are separately included above for when one decides to use only one in the propagator set.
--cttry b row  returns a row with bs where they should be added according to basic technique 3 and advanced technique 2.
--Note: This funcion should only be called if row lacks only one cell of type (cnot b).
cttry :: Cell -> Grid -> Row -> Row
cttry b grid row = [ if x == U && (elem (combinerows row try) grid || not (notriples (combinerows row try))) then b else U | (x, try) <- zip row (oneelt (cnot b) (length row)) ]

--ctrow b row  returns a row with bs where they should be added according to basic technique 3 and advanced technique 2.
ctrow :: Cell -> Grid -> Row -> Row
ctrow b grid row = 
  case succ (count (cnot b) row) == div (length row) 2 of
    False -> replicate (length row) U
    True -> cttry b grid row

--ctmask  creates a masker for basic technique 3 and advanced technique 2.
ctmask :: Masker
ctmask grid b = let grid' = transpose grid in 
  combinemask (map (ctrow b grid) grid) (transpose (map (ctrow b grid') grid'))


--  Propagation  --
--makepropagator masker grid  propagates grid according to masker and sets the correct message
makepropagator :: Masker -> Propagator
makepropagator masker grid =
  let [x,o] = map (masker grid) [X, O] in
    case masksok x o of
      False -> (grid, Failed)
      True -> case allunknown x && allunknown o of
        True -> (grid, AtFixpt)
        False -> let grid' = apply2 grid x o in
          case correct grid' of
            True -> (grid', Unknown)
            False -> (grid, Failed)

--propagator  is used to refer to different propagators by their index numbers.
propagator :: Int -> Propagator
propagator 1 = makepropagator basic1mask
propagator 2 = makepropagator basic2mask
propagator 3 = makepropagator basic3mask
propagator 4 = makepropagator basic4mask
propagator 7 = makepropagator adv2mask
propagator 8 = makepropagator ctmask

--propagators  is the list of propagators to use
propagators :: [Int]
propagators = [1,2,4,8]

--propagaterec R Q s s'  is a recursive implementation of the propagation algoritm (see the report)
--We assume that every propagator must be rescheduled if one propagator changes the grid
propagaterec :: [Int] -> [Int] -> Grid -> Maybe Grid
propagaterec r [] grid = Just grid
propagaterec r q grid =
  let (grid', msg) = propagator (head q) grid in
    case msg of
      Failed -> Nothing
      Unknown -> propagaterec r r grid'
      Subsumed -> propagaterec (delete (head q) r) (tail q) grid'
      AtFixpt -> propagaterec r (tail q) grid'

--propagate grid  propagates grid using the propagators from the list
propagate :: Grid -> Maybe Grid
propagate grid = propagaterec propagators propagators grid


--  Search  --
--bestrow grid  returns the index of the row with the least non-zero amount of Us.
--Note: The value 100 should in general be larger than the maximum width of puzzles.
--This metric is used to determine the 'best row' to guess new values.
bestrow :: Grid -> Int
bestrow grid = let xs = [ length (filter (== U) row) | row <- grid ] in
  let ys = [ if i == 0 then 100 else i | i <- xs ] in
    head $ filter ((== minimum ys) . (ys !!)) [0..]

--guessrow b row  replaces the first occurence of U in row by b.
guessrow :: Cell -> Row -> Row
guessrow b [] = []
guessrow b (x:xs)
  | x == U = b : xs
  | otherwise = x : guessrow b xs

--guessi i b grid  replaces the first occurence of U in the i'th row of grid by b.
guessi :: Int -> Cell -> Grid -> Grid
guessi 0 b (x:xs) = guessrow b x : xs
guessi n b (x:xs) = x : guessi (n-1) b xs

--guess b grid  calculates the best row to place a new value and places b there
guess :: Cell -> Grid -> Grid
guess b grid = guessi (bestrow grid) b grid


--  Solving  --
--solve grid  solves grid by first propagating and then trying new values for empty spots.
solve :: Grid -> Maybe Grid
solve grid = propagate grid >>= branch

--branch grid  returns Just grid if grid is solved. If that is not the case, it tries to solve grid with one newly placed X. If that does not work, it tries one newly placed O. If that doesn't work, it returns Nothing.
branch :: Grid -> Maybe Grid
branch grid = do
  if solved grid then Just grid else
    let grid' = solve $ guess X grid in
      case grid' of
        Just solution -> Just solution
        Nothing -> solve $ guess O grid


--  I/O  --
input2Cell :: Char -> Cell
input2Cell '.' = U
input2Cell 'X' = X
input2Cell 'O' = O

input2Grid :: [String] -> Grid
input2Grid l = map (map input2Cell) l

showCell :: Cell -> Char
showCell U = '.'
showCell X = 'X'
showCell O = 'O'

showGrid :: Grid -> String
showGrid [row] = map showCell row
showGrid (row:rows) = map showCell row ++ ('\n':showGrid rows)

--inputcheck grid r c  checks whether grid has r rows consisting of c elements
inputCheck :: Grid -> Int -> Int -> Maybe Grid
inputCheck grid rows cols
 | rows == length grid && and (map ((==) cols . length) grid) = Just grid
 | True = Nothing


--  Main  --
--main  prints the solution to the puzzle given on stdin.
main :: IO ()
main = do
  options <- getLine
  body <- fmap lines getContents
  let [rows, cols] = map read $ words options :: [Int] in
    case inputCheck (input2Grid body) rows cols >>= solve of
      Nothing -> putStrLn "No solution"
      Just grid -> putStrLn $ showGrid grid

--mainfile filename  prints the solution to the puzzle given in filename.
--This can be used in the repl.
mainfile :: FilePath -> IO ()
mainfile name = do
  (options:body) <- fmap lines $ readFile name
  let [rows, cols] = map read $ words options :: [Int] in
    case inputCheck (input2Grid body) rows cols >>= solve of
      Nothing -> putStrLn "No solution"
      Just grid -> putStrLn $ showGrid grid



---  Testing  ---
--Some puzzles and functions used for testing
example = transpose [[X,U,U,U,O,U],[U,U,O,U,O,X],[U,O,O,U,U,U],[O,O,U,U,X,U],[U,U,U,U,U,O],[U,X,X,U,U,O]] -- should be solvable using basic 1,2
example2 = input2Grid ["O....X","O.O..X",".X.O..","...O.X","......","..O.X."] -- should be solvable using basic 1,2,4, but doesn't have unique rows
example3 = input2Grid ["O.....","X.XX..","...X..",".X...O",".....X",".X.O.."] -- basic 1,2,3,4 is not enough to solve this

fail1ex = [[O, O, U], [U, U, X], [U, U, X]] -- basic 1 should fail on this
fail2ex = [[U, O, U], [X, U, X], [U, O, U]] -- basic 2 should fail on this

ex12 = input2Grid ["......X...O.","XX...O...X..","X...X..O....","..OO.....X..","O....X.X....","...O.....X..","...O......O.",".....O.O....","O...X...X..X","....OO...O.X","...O........",".X..X......X"]
gX4'  = input2Grid [ "....XO.......X", "X....O...X.X.X", "..X...X.......", "O....XX....O.X", "O.O.....X....X", ".O......O.....", "..X......X..X.", "..X.O..X..O...", ".....X.X....X.", "O.XO.X....O...", "..X.......OO..", "...O..........", ".X.......O....", "..O...O....O.."]

--p and pr can be used to pretty print grids and propagations.
p e = putStrLn $ showGrid e

pr (e,m) = do
 putStrLn $ show m
 p e
