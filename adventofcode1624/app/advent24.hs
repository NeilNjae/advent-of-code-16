{-# LANGUAGE DeriveGeneric #-}

module Main(main) where

import GHC.Generics (Generic)
import Data.Maybe (catMaybes, fromJust)
import Data.List 
import Data.Char (isDigit)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import qualified Data.HashSet as S
import Data.Graph.AStar
import Data.Hashable
import qualified Data.HashSet
import Debug.Trace

type Point = (Int, Int) -- row, column, both zero based.
type Grid = M.Map Point Char

data Path = Path { from :: Point
                 , to :: Point
                 , path :: Int
                 } deriving (Eq, Generic)
instance Hashable Path
-- instance Eq Path where
--     p1 == p2 = (joins p1) == (joins p2) 
instance Show Path where
    show p = "Path { " ++ show (from p) ++ " -> " ++ show (to p) ++ ", " ++ show (path p) ++ " }"

type PathSet = S.HashSet Path

-- Grid search state
data Gss = Gss { origin :: Point
               , current :: Point
               , goal :: Point
               , gssPath :: [Point]
               } deriving (Eq, Generic)
instance Hashable Gss
instance Ord Gss where
    s1 `compare` s2 = ((heuristicG s1) + (length (gssPath s1))) `compare` ((heuristicG s2) + (length (gssPath s2)))
instance Show Gss where
    show gss = "Gss { " ++ show (origin gss) ++ " -> " ++ show (current gss) ++ " -> " ++ show (goal gss) ++ " }"

data Tour = Tour {at :: Point, tour :: [Path]} deriving (Show, Eq)


littleGrid = "\
\###########\n\
\#0.1.....2#\n\
\#.#######.#\n\
\#4.......3#\n\
\###########"

main :: IO ()
main = do 
    text <- readFile "data/advent24.txt" 
    -- let text = littleGrid
    let tl = lines text
    let mp = M.fromList $ [((r, c), (tl!!r)!!c)| r <- [0..(length tl - 1)], c <- [0..(length (tl!!0) - 1)]]
    let goals = M.filter (isDigit) mp
    let start = head $ M.keys $ M.filter (=='0') mp
    let aStarSearch s g = asG [startGss s g] mp []
    let paths = map (fromGss) $ catMaybes $ [aStarSearch st gl | st <- (M.keys goals), gl <- (M.keys goals), st /= gl]

    -- part0 mp
    part1 start (M.keys goals) paths
    part2 start (M.keys goals) paths

part0 mp = print (length allPaths, allPaths)
    where goals = M.filter (isDigit) mp
          start = head $ M.keys $ M.filter (=='0') mp
          -- goal = head $ M.keys goals
          goal = head $ M.keys $ M.filter (=='2') mp
          -- aStarSearch s g = aStar (successorsG mp) costG heuristicG isGoalG (startGss s g)
          aStarSearch s g = asG [startGss s g] mp []
          traceSearch s g = trace ("Trace " ++ (show s) ++ " -> " ++ (show g)) (aStarSearch s g)
          paths = S.fromList $ map (fromGss) $ catMaybes $ map (\gl -> aStarSearch start gl) $ M.keys goals
          path = aStarSearch start goal
          allPaths = map (fromGss) $ catMaybes $ [traceSearch st gl | st <- (M.keys goals), gl <- (M.keys goals), st /= gl]

fromGss :: Gss -> Path
fromGss g = Path {from = origin g, to = goal g, path = (length (gssPath g)) - 1}

part1 :: Point -> [Point] -> [Path] ->  IO ()
part1 start points paths = print $ shortestTour start points paths

    -- where goals = M.filter (isDigit) mp
    --       start = head $ M.keys $ M.filter (=='0') mp
    --       aStarSearch s g = asG [startGss s g] mp []
    --       paths = nub $ map (fromGss) $ catMaybes $ [aStarSearch st gl | st <- (M.keys goals), gl <- (M.keys goals), st /= gl]

part2 :: Point -> [Point] -> [Path] ->  IO ()
part2 start points paths = print $ shortestReturningTour start points paths

-- bfsG :: [Gss] -> Grid -> [Path] -> [Point] -> [Path]
-- -- bfsG ps _ paths closed | trace ((show ps) ++ " :: " ++ (show paths) ++ " X " ++ (show closed)) False = undefined
-- bfsG [] _ paths _ = paths
-- bfsG (p:ps) g paths closed = 
--     if current p /= origin p && isDigit currentCell
--         then bfsG ps g (newPath:paths) ((current p):closed)
--         else bfsG (ps ++ extraAgenda) g paths ((current p):closed)
--     where currentCell = (g!(current p))
--           newPath = Path {joins = S.fromList [current p, origin p], path = gssPath p}
--           nextPoints = filter (\np -> np `notElem` ((gssPath p) ++ closed)) $ gridNeighbours g $ current p
--           extraAgenda = map (\n -> Gss {origin = origin p, goal = goal p, current = n, gssPath = (n:(gssPath p))}) nextPoints

asG :: [Gss] -> Grid -> [Point] -> Maybe Gss
-- asG ps _ closed | trace ((show ps) ++ " :: " ++ (show paths) ++ " X " ++ (show closed)) False = undefined
asG [] _ _ = Nothing
asG (p:ps) g closed = 
    if current p == goal p
        then Just p
        else if (head $ gssPath p) `elem` closed
                then asG ps g closed
                else asG newAgenda g ((current p):closed)
    where -- currentCell = (g!(current p))
          -- newPath = Path {joins = S.fromList [current p, origin p], path = gssPath p}
          nextPoints = filter (\np -> np `notElem` ((gssPath p) ++ closed)) $ gridNeighbours g $ current p
          extraAgenda = map (\n -> Gss {origin = origin p, goal = goal p, current = n, gssPath = (n:(gssPath p))}) nextPoints
          newAgenda = sort (ps ++ extraAgenda)


startGss :: Point -> Point -> Gss
startGss p g = Gss {origin = p, goal = g, current = p, gssPath = [p]}


gridNeighbours :: Grid -> Point -> [Point]
gridNeighbours g p@(r, c) = filter (\n -> (g!n) /= '#') ns
    where ns = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

heuristicG :: Gss -> Int
heuristicG gss = abs (rg - rc) + abs (cg - cc)
    where (rg, cg) = goal gss
          (rc, cc) = current gss

-- isGoalG :: Gss -> Bool
-- isGoalG st = current st == goal st

-- successorsG :: Grid -> Gss -> Data.HashSet.HashSet Gss
-- successorsG grid st = Data.HashSet.fromList extraAgenda
--     where nextPoints = gridNeighbours grid $ current st
--           -- nextPoints = filter (\np -> np `notElem` (gssPath st)) $ gridNeighbours grid $ current st
--           extraAgenda = map (\n -> Gss {origin = origin st, goal = goal st, current = n, gssPath = (n:(gssPath st))}) nextPoints

-- costG :: a -> b -> Int
-- costG = const $ const 1



shortestTour :: Point -> [Point] -> [Path] -> Int
shortestTour p0 points paths = minimum $ map (\p -> pathLength p paths) startRight
    where pointPerms = permutations points
          startRight = filter (\p -> (head p) == p0) pointPerms

shortestReturningTour :: Point -> [Point] -> [Path] -> Int
shortestReturningTour p0 points paths = minimum $ map (\p -> pathLength p paths) startRight
    where pointPerms = map (\p -> p ++ [p0]) $ permutations points
          startRight = filter (\p -> (head p) == p0) pointPerms

pathBetween :: [Path] -> Point -> Point -> Path
pathBetween paths a b = head $ filter (\p -> from p == a && to p == b) paths

adjacents :: [a] -> [[a]]
adjacents ts = filter (\t -> (length t) == 2) $ map (\t -> take 2 t) $ tails ts

pathLength :: [Point] -> [Path] -> Int
pathLength points paths = sum $ map (path) builtPath
  where pairs = adjacents points
        builtPath = foldl (addPath paths) [] pairs

addPath :: [Path] -> [Path] -> [Point] -> [Path]
addPath paths built posPair = built ++ [toAdd]
    where toAdd = pathBetween paths (posPair!!0) (posPair!!1)


