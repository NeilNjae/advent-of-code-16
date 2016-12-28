-- Using the idea of canonical representation of buildings from
-- https://andars.github.io/aoc_day11.html by Andrew Foote,
-- plus my extension of represening the pairs as an integer.

-- This version is A* search, using a priority queue for the agenda
-- and HashSets for various collecions.

{-# LANGUAGE DeriveGeneric #-}

module Main(main) where

import GHC.Generics (Generic)

-- import Prelude hiding (length, take, drop)
import Data.List (subsequences, (\\), sort, sortOn, nub, findIndices)
import Data.Ord (comparing)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import qualified Data.PQueue.Prio.Min as P
import Data.Hashable
import qualified Data.HashSet as S
import qualified Data.Sequence as Q
import Data.Sequence ((<|), (|>), (><))
import Data.Foldable (toList, foldr')


data Item = Generator String | Microchip String deriving (Show, Eq, Generic)
instance Hashable Item
type Floor = [Item]
data Building = Building Int [Floor] deriving (Show, Eq, Generic)
instance Hashable Building
data CBuilding = CBuilding Int Integer deriving (Show, Eq, Generic)
instance Hashable CBuilding
type CBuildings = S.HashSet CBuilding
data Agendum = Agendum {current :: Building, trail :: Q.Seq CBuilding, cost :: Int} deriving (Show, Eq)
type Agenda = P.MinPQueue Int Agendum 
type Candidates = S.HashSet (Int, Agendum)

instance Ord Item where
    compare (Generator a) (Generator b) = compare a b
    compare (Microchip a) (Microchip b) = compare a b
    compare (Generator _) (Microchip _) = LT
    compare (Microchip _) (Generator _) = GT

instance Ord Building where
    compare b1 b2 = comparing estimateCost b1 b2

building1 = Building 0 [
    (sort [Generator "polonium", Generator "thulium", 
     Microchip "thulium", Generator "promethium", Generator "ruthenium",
     Microchip "ruthenium", Generator "cobalt", Microchip "cobalt"]),
    (sort [Microchip "polonium", Microchip "promethium"]),
    [],
    []
    ]

building0 = Building 0 [
    (sort [Generator "polonium", Generator "thulium", 
     Microchip "thulium", Generator "promethium"]),
    (sort [Microchip "polonium", Microchip "promethium"]),
    [],
    []
    ]

building2 = Building 0 [
    (sort [Generator "polonium", Generator "thulium", 
     Microchip "thulium", Generator "promethium", Generator "ruthenium",
     Microchip "ruthenium", Generator "cobalt", Microchip "cobalt",
     Generator "elerium", Microchip "elerium",
     Generator "dilithium", Microchip "dilithium"]),
    (sort [Microchip "polonium", Microchip "promethium"]),
    [],
    []
    ]


buildingTest = Building 0 [
    sort([Microchip "hydrogen", Microchip "lithium"]),
    [Generator "hydrogen"],
    [Generator "lithium"],
    []]

canonical :: Building -> CBuilding
canonical (Building f floors) = CBuilding f (read $ filter (isDigit) $ show $ sort pairs)
    where names = nub $ map (\(Generator n) -> n) $ filter (isGenerator) $ concat floors
          floorOf (Generator g) = head (findIndices 
                                                (\fl -> (Generator g) `elem` fl) 
                                                floors)
          floorOf (Microchip g) = head (findIndices 
                                                (\fl -> (Microchip g) `elem` fl) 
                                                floors)
          pairs = foldl (\ps n -> (floorOf (Generator n), floorOf (Microchip n)):ps) [] names


main :: IO ()
main = do 
    part1 
    part2 

part1 :: IO ()
part1 = print $ length $ trail $ fromMaybe (snd $ P.findMin $ initAgenda buildingTest) $ aStar (initAgenda building1) S.empty

part2 :: IO ()
part2 = print $ length $ trail $ fromMaybe (snd $ P.findMin $ initAgenda buildingTest) $ aStar (initAgenda building2) S.empty
initAgenda :: Building -> Agenda
initAgenda b = P.singleton (estimateCost b) Agendum {current = b, trail = Q.empty, cost = estimateCost b}


aStar :: Agenda -> CBuildings -> Maybe Agendum
-- aStar [] _ = Agendum {current=buildingTest, trail=[], cost=0}
aStar agenda closed 
    | P.null agenda = Nothing
    | otherwise = 
        if isGoal reached then Just currentAgendum
        else if creached `S.member` closed 
            then aStar (P.deleteMin agenda) closed
            else aStar newAgenda (S.insert creached closed)
        where 
            (_, currentAgendum) = P.findMin agenda
            reached = current currentAgendum
            creached = canonical reached
            newAgenda = foldr' (\(c, a) q -> P.insert c a q) (P.deleteMin agenda) $ candidates currentAgendum closed
            -- newAgenda = P.union (P.deleteMin agenda) 
            --                     (P.fromList $ toList $ candidates currentAgendum closed)


candidates :: Agendum -> CBuildings -> Q.Seq (Int, Agendum)
candidates agendum closed = newCandidates
    where
        candidate = current agendum
        previous = trail agendum
        succs = legalSuccessors $ successors candidate
        nonloops = Q.filter (\s -> not $ (canonical s) `S.member` closed) succs
        newCandidates = fmap (\a -> (cost a, a)) $ fmap (\n -> makeAgendum n) nonloops
        makeAgendum new = Agendum {current = new, 
                                    trail = (canonical candidate) <| previous, 
                                    cost = estimateCost new + length previous + 1}

isGoal :: Building -> Bool
isGoal (Building f floors) =
    f+1 == height && (all (null) $ take f floors)
    where height = length floors

isLegal :: Building -> Bool
isLegal (Building f floors) = 
    null floor 
    ||
    not (any (isGenerator) floor)
    ||
    any (safePair) pairs
    where floor = floors!!f
          pairs = [(i, j) | i <- floor, j <- floor, isGenerator i]
          safePair (Generator e, Microchip f) = e == f
          safePair (Generator _, Generator _) = False

isGenerator :: Item -> Bool
isGenerator (Generator _) = True
isGenerator (Microchip _) = False

successors :: Building -> (Q.Seq Building)
successors (Building f floors) = Q.fromList [updateBuilding f floors nf is | nf <- nextFloors, is <- items]
    where 
        floor = floors!!f
        items = filter (\is -> length is == 1 || length is == 2) $ subsequences floor
        nextFloors = if f == 0 then [1]
                     else if f+1 == length floors then [f-1]
                     else [f+1, f-1]

legalSuccessors :: (Q.Seq Building) -> (Q.Seq Building)
legalSuccessors = Q.filter (isLegal)

updateBuilding :: Int -> [Floor] -> Int -> [Item] -> Building
updateBuilding oldF oldFloors newF items = Building newF newFloors
    where newFloors = map (updateFloor) $ zip [0..] oldFloors
          updateFloor (f, fl) 
            | f == oldF = sort $ fl \\ items
            | f == newF = sort $ items ++ fl
            | otherwise = fl

estimateCost :: Building -> Int
estimateCost (Building _ floors) = 
    sum $ map (\(c, f) -> c * length f) $ zip [0..] $ reverse floors

