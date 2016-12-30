-- Using the idea of canonical representation of buildings from
-- https://andars.github.io/aoc_day11.html by Andrew Foote,
-- plus my extension of represening the pairs as an integer.

-- This version is A* search, using a priority queue for the agenda,
-- Sets for various collecions, and a Map to store the floors in the
-- building.

{-# LANGUAGE DeriveGeneric #-}

module Main(main) where

import GHC.Generics (Generic)

-- import Prelude hiding (length, take, drop)
import Data.List (subsequences, (\\), sort, sortOn, nub, findIndices, intercalate)
import Data.Ord (comparing)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.PQueue.Prio.Min as P
import qualified Data.HashSet as S
import qualified Data.Sequence as Q
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.Sequence ((<|), (|>), (><))
import Data.Foldable (toList, foldr', foldl', all)
import Debug.Trace

data Item = Generator String | Microchip String deriving (Eq, Generic)
instance Hashable Item
data Floor = Floor (S.HashSet Item) deriving (Eq, Generic)
instance Hashable Floor
unFloor :: Floor -> S.HashSet Item
unFloor (Floor f) = f
data Floors = Floors (M.HashMap Int Floor) deriving (Eq, Generic)
instance Hashable Floors
data Building = Building Int Floors deriving (Eq, Generic)
instance Hashable Building
type Buildings = S.HashSet Building
-- data CBuilding = CBuilding Int Integer deriving (Show, Eq, Generic)
-- instance Hashable CBuilding
-- type CBuildings = S.HashSet CBuilding
data Agendum = Agendum {current :: Building, trail :: Q.Seq Building, cost :: Int} deriving (Show, Eq)
type Agenda = P.MinPQueue Int Agendum 
type Candidates = S.HashSet (Int, Agendum)

instance Show Item where
    show (Generator a) = "G" ++ take 2 a
    show (Microchip a) = "M" ++ take 2 a

instance Ord Item where
    compare (Generator a) (Generator b) = compare a b
    compare (Microchip a) (Microchip b) = compare a b
    compare (Generator _) (Microchip _) = LT
    compare (Microchip _) (Generator _) = GT

-- instance Ord Building where
--     compare b1 b2 = comparing estimateCost b1 b2

instance Show Building where
    show (Building f (Floors floors)) = (show f) ++ "<* " ++ (intercalate "; " $ toList $ M.map (showFloor. unFloor) floors)
        where showFloor fl = intercalate ", " $ toList $ S.map (show) fl


-- building1 = Building 0 [
--     (sort [Generator "polonium", Generator "thulium", 
--      Microchip "thulium", Generator "promethium", Generator "ruthenium",
--      Microchip "ruthenium", Generator "cobalt", Microchip "cobalt"]),
--     (sort [Microchip "polonium", Microchip "promethium"]),
--     [],
--     []
--     ]

building1 = Building 0 (Floors $ M.fromList 
        [ (0, Floor $ S.fromList [Generator "polonium", Generator "thulium", 
                          Microchip "thulium", Generator "promethium", Generator "ruthenium",
                          Microchip "ruthenium", Generator "cobalt", Microchip "cobalt"])
        , (1, Floor $ S.fromList [Microchip "polonium", Microchip "promethium"])
        , (2, Floor $ S.empty )
        , (3, Floor $ S.empty )
        ])



building0 = Building 0 (Floors $ M.fromList 
        [ (0, Floor $ S.fromList [Generator "polonium", Generator "thulium", Microchip "thulium", Generator "promethium"])
        , (1, Floor $ S.fromList [Microchip "polonium", Microchip "promethium"])
        , (2, Floor $ S.empty )
        , (3, Floor $ S.empty )
        ])

building2 = Building 0 (Floors $ M.fromList 
        [ (0, Floor $ S.fromList [Generator "polonium", Generator "thulium", 
                          Microchip "thulium", Generator "promethium", Generator "ruthenium",
                          Microchip "ruthenium", Generator "cobalt", Microchip "cobalt",
                          Generator "dilithium", Microchip "dilithium"])
        , (1, Floor $ S.fromList [Microchip "polonium", Microchip "promethium"])
        , (2, Floor $ S.empty )
        , (3, Floor $ S.empty )
        ])

buildingTest = Building 0 (Floors $ M.fromList 
        [ (0, Floor $ S.fromList [Microchip "hydrogen", Microchip "lithium"])
        , (1, Floor $ S.fromList [Generator "hydrogen"])
        , (2, Floor $ S.fromList [Generator "lithium"])
        , (3, Floor $ S.empty )
        ])


main :: IO ()
main = do 
    -- part0
    part1 
    part2 

part0 :: IO ()
part0 = print $ length $ trail $ fromMaybe (snd $ P.findMin $ initAgenda buildingTest) $ aStar (initAgenda buildingTest) S.empty

part1 :: IO ()
part1 = print $ length $ trail $ fromMaybe (snd $ P.findMin $ initAgenda buildingTest) $ aStar (initAgenda building1) S.empty

part2 :: IO ()
part2 = print $ length $ trail $ fromMaybe (snd $ P.findMin $ initAgenda buildingTest) $ aStar (initAgenda building2) S.empty


initAgenda :: Building -> Agenda
initAgenda b = P.singleton (estimateCost b) Agendum {current = b, trail = Q.empty, cost = estimateCost b}


aStar :: Agenda -> Buildings -> Maybe Agendum
-- aStar [] _ = Agendum {current=buildingTest, trail=[], cost=0}
aStar agenda closed 
    -- | trace ("Peeping " ++ (show $ fst $ P.findMin agenda) ++ ": " ++ (show reached) ++ " <- " ++ (show $ toList $ Q.take 1 $ trail $ currentAgendum) ++ " :: " ++ (show newAgenda)) False = undefined
    | P.null agenda = Nothing
    | otherwise = 
        if isGoal reached then Just currentAgendum
        else if reached `S.member` closed 
            then aStar (P.deleteMin agenda) closed
            else aStar newAgenda (S.insert reached closed)
        where 
            (_, currentAgendum) = P.findMin agenda
            reached = current currentAgendum
            newAgenda = foldl' (\q a -> P.insert (cost a) a q) (P.deleteMin agenda) $ candidates currentAgendum closed



candidates :: Agendum -> Buildings -> Q.Seq Agendum
candidates agendum closed = newCandidates
    where
        candidate = current agendum
        previous = trail agendum
        succs = legalSuccessors $ successors candidate
        nonloops = Q.filter (\s -> not $ s `S.member` closed) succs
        newCandidates = fmap (\n -> makeAgendum n) nonloops
        makeAgendum new = Agendum {current = new, 
                                    trail = candidate <| previous, 
                                    cost = estimateCost new + length previous + 1}

isGoal :: Building -> Bool
isGoal (Building f (Floors floors)) =
    f+1 == height && (all (\fl -> S.null $ unFloor fl) $ M.filterWithKey (\k _ -> k < f) floors)
    where height = M.size floors

isLegal :: Building -> Bool
isLegal (Building f (Floors floors)) = 
    null floor 
    ||
    not (any (isGenerator) floor)
    ||
    any (safePair) pairs
    where floor = unFloor $ fromJust $ M.lookup f floors
          pairs = [(i, j) | i <- (S.toList floor), j <- (S.toList floor), isGenerator i]
          safePair (Generator e, Microchip f) = e == f
          safePair (Generator _, Generator _) = False

isGenerator :: Item -> Bool
isGenerator (Generator _) = True
isGenerator (Microchip _) = False

successors :: Building -> (Q.Seq Building)
successors b@(Building f (Floors floors)) = Q.fromList [updateBuilding b nf is | nf <- nextFloors, is <- items]
    where 
        floor = unFloor $ fromJust $ M.lookup f floors
        items = map (S.fromList) $ filter (\is -> length is == 1 || length is == 2) $ subsequences $ toList floor
        nextFloors = if f == 0 then [1]
                     else if f+1 == length floors then [f-1]
                     else [f+1, f-1]

legalSuccessors :: (Q.Seq Building) -> (Q.Seq Building)
legalSuccessors = Q.filter (isLegal)

updateBuilding :: Building -> Int -> S.HashSet Item -> Building
updateBuilding (Building oldF (Floors oldFloors)) newF items = Building newF (Floors newFloors)
    where oldFloorsE = fmap (unFloor) oldFloors
          newFloorsE = M.adjust (\f -> f `S.union` items) newF $ M.adjust (\f -> f `S.difference` items) oldF oldFloorsE
          newFloors = fmap (Floor) newFloorsE


estimateCost :: Building -> Int
estimateCost (Building _ (Floors floors)) = 
    sum $ map (\(c, f) -> c * S.size f) $ zip [0..] $ reverse $ M.elems $ fmap (unFloor) floors

