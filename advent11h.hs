-- Using the idea of canonical representation of buildings from
-- https://andars.github.io/aoc_day11.html by Andrew Foote,
-- plus my extension of represening the pairs as an integer.

-- This version is hillclimbing search, using a list for the agenda.

import Data.List (subsequences, (\\), sort, sortOn, nub, findIndices)
import Data.Ord (comparing)
import Data.Char (isDigit)

data Item = Generator String | Microchip String deriving (Show, Eq)
type Floor = [Item]
data Building = Building Int [Floor] deriving (Show, Eq)
data CBuilding = CBuilding Int Integer deriving (Show, Eq)
data Agendum = Agendum {current :: Building, trail :: [CBuilding], cost :: Int}

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
part1 = print $ length $ trail $ hillClimb (initAgenda building1) []

part2 :: IO ()
part2 = print $ length $ trail $ hillClimb (initAgenda building2) []

initAgenda :: Building -> [Agendum]
initAgenda b = [Agendum {current = b, trail=[], cost = estimateCost b}]

hillClimb :: [Agendum] -> [CBuilding] -> Agendum
hillClimb [] _ = Agendum {current=buildingTest, trail=[], cost=0}
hillClimb (currentAgendum:agenda) closed = 
    if isGoal reached then currentAgendum
    else if creached `elem` closed 
        then hillClimb agenda closed
        else hillClimb newAgenda (creached:closed) 
    where 
        reached = current currentAgendum
        creached = canonical reached
        newAgenda = 
            sortOn (cost) $ 
            agenda ++ (candidates currentAgendum closed)


candidates :: Agendum -> [CBuilding] -> [Agendum]
candidates agendum closed = newCandidates
    where
        candidate = current agendum
        previous = trail agendum
        succs = legalSuccessors $ successors candidate
        excludable = previous ++ closed
        nonloops = filter (\s -> not $ (canonical s) `elem` excludable) succs
        newCandidates = map (\n -> makeAgendum n) nonloops
        makeAgendum new = Agendum {current = new, 
                                    trail = (canonical candidate):previous, 
                                    cost = estimateCost new}

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

successors :: Building -> [Building]
successors (Building f floors) = [updateBuilding f floors nf is | nf <- nextFloors, is <- items]
    where 
        floor = floors!!f
        items = filter (\is -> length is == 1 || length is == 2) $ subsequences floor
        nextFloors = if f == 0 then [1]
                     else if f+1 == length floors then [f-1]
                     else [f+1, f-1]

legalSuccessors :: [Building] -> [Building]
legalSuccessors = filter (isLegal)

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

