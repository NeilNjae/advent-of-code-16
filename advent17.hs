import Data.ByteString.Char8 (pack)
import qualified Crypto.Hash as C

type Position = (Int, Int)
data Agendum = Agendum {position :: Position, path :: String, hash :: String} deriving (Show, Eq)
type Agenda = [Agendum]

-- input = "hijkl"
-- input = "ihgpwlah"

input = "qljzarfv" -- my input

main :: IO ()
main = do 
    part1 
    part2 

part1 :: IO ()
part1 = print $ path $ extractJust $ bfs initialAgenda

part2 :: IO ()
part2 = print $ bfs2 initialAgenda 0

initialAgenda :: Agenda
initialAgenda = [Agendum {position=(1, 1), path="", hash=(getHash "")}]

getHash :: String -> String
getHash path = show (C.hash $ pack (input ++ path) :: C.Digest C.MD5)

extractJust :: Maybe Agendum -> Agendum
extractJust Nothing = head initialAgenda
extractJust (Just x) = x

bfs :: Agenda -> Maybe Agendum
bfs [] = Nothing
bfs (current:agenda) = 
    if isGoal current then Just current
    else bfs (agenda ++ (successors current))

bfs2 :: Agenda -> Int -> Int
bfs2 [] l = l
bfs2 (current:agenda) l = 
    if isGoal current then bfs2 agenda (length $ path $ current)
    else bfs2 (agenda ++ (successors current)) l

isGoal :: Agendum -> Bool
isGoal agendum = (position agendum) == (4, 4)

isLegalPos :: Position -> Bool
isLegalPos p = fst p >= 1 && fst p <= 4 && snd p >= 1 && snd p <= 4

successors :: Agendum -> Agenda
successors state = [Agendum {position = step p0 ld, 
                             path = path0 ++ [ld],
                             hash = getHash (path0 ++ [ld])} | ld <- legalDoors ]
    where 
        p0 = position state
        path0 = path state
        h0 = hash state
        doors = openDoors h0
        legalDoors = filter (isLegalPos . (step p0)) doors

openDoors :: String -> String
openDoors h = up ++ down ++ left ++ right
    where
        up    = if h!!0 `elem` "bcdef" then "U" else ""
        down  = if h!!1 `elem` "bcdef" then "D" else ""
        left  = if h!!2 `elem` "bcdef" then "L" else ""
        right = if h!!3 `elem` "bcdef" then "R" else ""

step :: Position -> Char -> Position
step (r, c) 'U' = (r-1, c)
step (r, c) 'D' = (r+1, c)
step (r, c) 'L' = (r, c-1)
step (r, c) 'R' = (r, c+1)
