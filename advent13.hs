import Data.List ((\\), nub, sortOn)
import Data.Bits (popCount)

type Pos = (Int, Int)

seed = 1362

goal1 = (31, 39)

main :: IO ()
main = do 
    part1 
    part2 


part1 :: IO ()
part1 = print $ length $ tail $ extractJust $ aStar [[(1, 1)]] []

part2 :: IO ()
part2 = do print $ length $ tail $ edl 50 [[(1, 1)]] []
           putStrLn $ showRoomR 30 25 $ edl 50 [[(1, 1)]] []


extractJust :: Maybe [a] -> [a]
extractJust Nothing = []
extractJust (Just x) = x

isWall :: Int -> Int -> Bool
isWall x y = (popCount n) `mod` 2 == 1
    where 
        n = x*x + 3*x + 2*x*y + y + y*y + seed


showRoom w h = showRoomR w h []

showRoomR w h reached = unlines rows
    where 
        rows = [row x | x <- [0..h]]
        row x = [showCell x y | y <- [0..w]]
        showCell x y = if (isWall x y) 
            then '#' 
            else if (x, y) `elem` reached 
                then 'O'
                else '.'


aStar :: [[Pos]] -> [Pos] -> Maybe [Pos]
aStar [] _ = Nothing
aStar (currentTrail:trails) closed = 
    if isGoal (head currentTrail) then Just currentTrail
    else if (head currentTrail) `elem` closed then aStar trails closed
         else aStar newAgenda ((head currentTrail): closed) 
    where newAgenda = 
            sortOn (\a -> trailCost a) $ 
            trails ++ (candidates currentTrail closed)
          trailCost t = estimateCost (head t) + length t - 1


-- exhaustive depth-limited
edl :: Int -> [[Pos]] -> [Pos] -> [Pos]
edl _ [] closed = nub closed
edl limit (currentTrail:trails) closed = 
    if (length currentTrail) > (limit+1) then edl limit trails ((head currentTrail):closed)
    else if (head currentTrail) `elem` closed then edl limit trails closed
         else edl limit newAgenda ((head currentTrail):closed) 
    where newAgenda = trails ++ (candidates currentTrail closed)

candidates :: [Pos] -> [Pos] -> [[Pos]]
candidates currentTrail closed = newCandidates
    where
        (candidate:trail) = currentTrail
        succs = legalSuccessors $ successors candidate
        nonloops = (succs \\ trail) \\ closed
        newCandidates = map (\n -> n:candidate:trail) nonloops

isGoal :: Pos -> Bool
isGoal p = p == goal1

isLegal :: Pos -> Bool
isLegal (x, y) = 
    x >= 0 && y >= 0 && (not $ isWall x y)

successors :: Pos -> [Pos]
successors (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

legalSuccessors :: [Pos] -> [Pos]
legalSuccessors = filter (isLegal)

estimateCost :: Pos -> Int
estimateCost (x, y) = abs (x - gx) + abs (y - gy)
    where (gx, gy) = goal1

