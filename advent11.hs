import Data.List (subsequences, (\\), sort, sortBy)
import Data.Ord (comparing)

data Item = Generator String | Microchip String deriving (Show, Eq)
type Floor = [Item]
data Building = Building Int [Floor] deriving (Show, Eq)

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

main :: IO ()
main = do 
    part1 
    part2 


part1 :: IO ()
-- part1 = print $ length $ init $ extractJust $ aStar [[buildingTest]] []
part1 = print $ length $ init $ extractJust $ aStar [[building1]] []

part2 :: IO ()
part2 = print $ length $ init $ extractJust $ aStar [[building2]] []


extractJust :: Maybe [a] -> [a]
extractJust Nothing = []
extractJust (Just x) = x

aStar :: [[Building]] -> [Building] -> Maybe [Building]
aStar [] _ = Nothing
aStar (currentTrail:trails) closed = 
    if isGoal (head currentTrail) then Just currentTrail
    else aStar newAgenda ((head currentTrail): closed) 
    where newAgenda = 
            sortBy (\t1 t2 -> (head t1) `compare` (head t2)) $ 
            trails ++ (candidates currentTrail closed)

candidates :: [Building] -> [Building] -> [[Building]]
candidates currentTrail closed = newCandidates
    where
        (candidate:trail) = currentTrail
        succs = legalSuccessors $ successors candidate
        nonloops = (succs \\ trail) \\ closed
        newCandidates = map (\n -> n:candidate:trail) nonloops

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
-- updateBuilding f floors _ _ = Building f floors
updateBuilding oldF oldFloors newF items = Building newF newFloors
    where numberedFloors = zip [0..] oldFloors
          newFloors = map (updateFloor) numberedFloors
          updateFloor (f, fl) 
            | f == oldF = sort $ fl \\ items
            | f == newF = sort $ items ++ fl
            | otherwise = fl

estimateCost :: Building -> Int
estimateCost (Building _ floors) = 
    sum $ map (\(c, f) -> c * length f) $ zip [0..] $ reverse floors

