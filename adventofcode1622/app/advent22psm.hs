{-# LANGUAGE DeriveGeneric #-}

module Main(main) where

import GHC.Generics (Generic)
import Text.Parsec 
import Text.ParserCombinators.Parsec.Number
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.List (find, delete, sort)
import Data.Foldable (foldr')
import Data.Hashable
import qualified Data.HashSet as S
import qualified Data.Sequence as Q
import Data.Sequence ((<|), (|>), (><))
import qualified Data.PQueue.Prio.Min as P
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict ((!))

import Debug.Trace


data Node = Node { size :: Int
                 , used :: Int
                 , available :: Int
                 , use_pc :: Int 
                 } deriving (Show, Eq, Ord, Generic)
instance Hashable Node
type NodeLocation = (Int, Int)
type NodeMap = M.HashMap NodeLocation Node 

data SearchState = SearchState { targetLoc :: NodeLocation 
                               , holeLoc :: NodeLocation
                               , grid :: NodeMap
                               , trail :: Q.Seq CSearchState
                               } deriving (Generic)
instance Hashable SearchState where
    hashWithSalt salt s = hashWithSalt salt (canonical s)
instance Eq SearchState where
    s1 == s2 = canonical s1 == canonical s2
instance Show SearchState where
    show s = "Search {" ++ (show $ canonical s) ++ " ; " ++ (show $ trail s) ++ "}"

type CSearchState = (NodeLocation, NodeLocation)
type CSearchStates = S.HashSet CSearchState
type Agenda = P.MinPQueue Int SearchState


canonical :: SearchState -> CSearchState
canonical s = (targetLoc s, holeLoc s)

testGrid = "\
\Filesystem            Size  Used  Avail  Use%\n\
\/dev/grid/node-x0-y0   10T    8T     2T   80%\n\
\/dev/grid/node-x0-y1   11T    6T     5T   54%\n\
\/dev/grid/node-x0-y2   32T   28T     4T   87%\n\
\/dev/grid/node-x1-y0    9T    7T     2T   77%\n\
\/dev/grid/node-x1-y1    8T    0T     8T    0%\n\
\/dev/grid/node-x1-y2   11T    7T     4T   63%\n\
\/dev/grid/node-x2-y0   10T    6T     4T   60%\n\
\/dev/grid/node-x2-y1    9T    8T     1T   88%\n\
\/dev/grid/node-x2-y2    9T    6T     3T   66%\n\
\"

main :: IO ()
main = do 
    text <- readFile "data/advent22.txt" 
    let sizes = M.fromList $ successfulParse $ parseFile text
    part1 sizes
    part2 sizes
    -- part2 $ M.fromList $ successfulParse $ parseFile testGrid

part1 :: NodeMap ->  IO ()
part1 sizes = print $ length viable
    where viable = [(a, b) | a <- M.keys sizes, 
                             b <- M.keys sizes, 
                             a /= b,
                             (used $ sizes!a) > 0,
                             (used $ sizes!a) <= (available $ sizes!b)]


part2 :: NodeMap ->  IO ()
part2 sizes = print $ length $ trail $ fromMaybe (snd $ P.findMin $ initAgenda sizes) $ aStar (initAgenda sizes) S.empty

initAgenda :: NodeMap -> Agenda
initAgenda nodes = P.singleton (heuristic st) st
    where st = startSt nodes

aStar :: Agenda -> CSearchStates -> Maybe SearchState
aStar agenda closed 
    -- | trace ("Peeping " ++ (show $ fst $ P.findMin agenda) ++ ": " ++ (show current) ++ " :: " ++ (show newAgenda)) False = undefined
    | P.null agenda = Nothing
    | otherwise = 
        if isGoal current then Just current
        else if creached `S.member` closed 
            then aStar (P.deleteMin agenda) closed
            else aStar newAgenda (S.insert creached closed)
        where 
            (_, current) = P.findMin agenda
            creached = canonical current
            newAgenda = foldr' (\(c, a) q -> P.insert c a q) (P.deleteMin agenda) $ candidates current closed


-- searchTrace :: [SearchState] -> String
-- searchTrace ss = unlines $ map (sst) ss
--     where sst s = "(" ++ show (tx s) ++ ", " ++ show (ty s) ++ ") :: " ++ holeS s
--           hole sk = fromJust $ find (\n -> used n == 0) $ grid sk 
--           holeS sk = "(" ++ show (x $ hole sk) ++ ", " ++ show (y $ hole sk) ++ ")"

startSt :: NodeMap -> SearchState
startSt nodes = SearchState { targetLoc = (maximum xs, 0)
                            , holeLoc = hole
                            , trail = Q.empty
                            , grid = nodes
                            }
    where xs = map (\n -> fst n) (M.keys nodes)
          hole = holeLocation nodes

holeLocation :: NodeMap -> NodeLocation
holeLocation nodes = head $ M.keys $ M.filter (\n -> used n == 0) nodes

isGoal :: SearchState -> Bool
isGoal st = targetLoc st == (0, 0)

heuristic :: SearchState -> Int
heuristic st = (tx + ty) + (abs (hx - tx)) + (abs (hy - ty)) - 1
    where (tx, ty) = targetLoc st
          (hx, hy) = holeLoc st

adjacent :: NodeLocation -> NodeLocation -> Bool
adjacent (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) == 1

-- A move of data from n1 to n2 is legal.
legal :: NodeMap -> NodeLocation -> NodeLocation -> Bool
legal nodes n1 n2 = adjacent n1 n2 && (used $ nodes!n1) > 0 && (used $ nodes!n1) <= (available $ nodes!n2)

candidates :: SearchState -> CSearchStates -> S.HashSet (Int, SearchState)
candidates st closed = newCandidates
    where
        previous = trail st
        succs = successors st
        nonloops = S.filter (\s -> not $ (canonical s) `S.member` closed) succs
        cost s = heuristic s + (Q.length $ trail s)
        newCandidates = S.map (\a -> (cost a, a)) nonloops

successors :: SearchState -> S.HashSet SearchState
successors st = S.fromList $ map (newState st) possibleMoves
    where nodes = grid st
          h = holeLoc st
          possibleMoves = [(h, n) | n <- (M.keys nodes), legal nodes n h]


-- Moving hole from h to h'
newState :: SearchState -> (NodeLocation, NodeLocation) -> SearchState
newState st (h, h') = candidate
    where candidate = st {targetLoc = t', holeLoc = h', trail = trail', grid = grid'}
          t = targetLoc st
          t' = if t == h' then h else t
          trail' = (canonical st) <| (trail st)
          u = used ((grid st)!h')
          u' = used ((grid st)!h)
          a = size((grid st)!h) - used ((grid st)!h')
          a' = size((grid st)!h') - used ((grid st)!h)
          grid' = M.adjust (\n -> n {used = u, available = a}) h $ M.adjust (\n -> n {used = u', available = a'}) h' $ grid st


--
-- Parsing
--

duFile = duLine `sepEndBy` newline 
-- duLine = (optionMaybe nodeL)

duLine = (nodeL >>= return . Just) <|> (headerL >> return Nothing)

headerL = (many (noneOf "\r\n"))

nodeL = nodeify <$> (string "/dev/grid/node-x" *> int)
                <*> (string "-y" *> int)
                <*> (spaces *> int <* string "T")
                <*> (spaces *> int <* string "T")
                <*> (spaces *> int <* string "T")
                <*> (spaces *> int <* string "%")
            where nodeify x y size used available use_pc = 
                      ((x, y), Node {size=size, used=used, available=available, use_pc=use_pc})

parseFile :: String -> Either ParseError [Maybe (NodeLocation, Node)]
parseFile input = parse duFile "(unknown)" input

parseLine :: String -> Either ParseError (Maybe (NodeLocation, Node))
parseLine input = parse duLine "(unknown)" input

successfulParse :: Either ParseError [Maybe a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = catMaybes a
