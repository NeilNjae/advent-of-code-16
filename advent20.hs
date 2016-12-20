import Text.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number
import Control.Applicative ((<$), (<*), (*>), (<*>), liftA)
import Data.List (foldl')

data Interval = Interval Int Int deriving (Show, Eq)

low :: Interval -> Int
low (Interval l _) = l

high :: Interval -> Int
high (Interval _ h) = h

-- input = 5 
input = 3012210 

main :: IO ()
main = do 
    text <- readFile "advent20.txt" 
    let intervals = successfulParse $ parseIfile text
    part1 intervals
    -- part2

part1 :: [Interval] -> IO ()
part1 intervals = print $ (+1) $ high $ head $ foldl' (mergeAdjacent) [] $ foldl' (merge) [] intervals

part2 :: [Interval] -> IO ()
part2 intervals = do
    let ints = foldl' (mergeAdjacent) [] $ foldl' (merge) [] intervals
    let gapCount = gaps ints
    let lowGap = max [0, ((low $ head ints) -1)]
    let highGap = 4294967295 - (min [4294967295, ((high $ last ints) + 1)])
    print (lowGap + gapCount + highGap)

-- 4294967295


disjoint :: Interval -> Interval -> Bool
disjoint (Interval a b) (Interval c d)
    | b < c = True
    | d < a = True
    | a > d = True
    | c > b = True
    | otherwise = False

intersect :: Interval -> Interval -> Bool
intersect a b = not $ disjoint a b

merge :: [Interval] -> Interval -> [Interval]
merge [] i0 = [i0]
merge (i1:intervals) i0
    | (high i0) < (low i1) = i0:i1:intervals
    | intersect i0 i1 = merge intervals (Interval a' b')
    | otherwise = i1:(merge intervals i0)
        where a' = minimum [low i0, low i1]
              b' = maximum [high i0, high i1]

mergeAdjacent :: [Interval] -> Interval -> [Interval]
mergeAdjacent [] i0 = [i0]
mergeAdjacent (i1:intervals) i0
    | high i0 + 1 == low i1 = (Interval (low i0) (high i1)):intervals
    | low i0 == high i1 + 1 = (Interval (low i1) (high i0)):intervals
    | otherwise = i1:(mergeAdjacent intervals i0)


gaps :: [Interval] -> Int
gaps [] = 0
gaps [_] = 0
gaps ((Interval _ b):(Interval c d):intervals) = 
    (c - b - 1) + gaps ((Interval c d):intervals)


intervalFile = intervalLine `endBy` newline 
-- IntervalLine = choice [cpyL, incL, decL, jnzL]
intervalLine = Interval <$> int <*> (string "-" *> int)


parseIfile :: String -> Either ParseError [Interval]
parseIfile input = parse intervalFile "(unknown)" input

parseIline :: String -> Either ParseError Interval
parseIline input = parse intervalLine "(unknown)" input

successfulParse :: Either ParseError [a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = a
