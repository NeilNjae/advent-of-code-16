import Text.Parsec
import Control.Applicative ((<$), (<*), (*>), liftA)
import Data.List (partition, union, intersect)

data Chunk = Include String | Exclude String deriving (Show)
data ChunkV = Includev Bool | Excludev Bool deriving (Show)

chunkValue :: Chunk -> String
chunkValue (Include v) = v
chunkValue (Exclude v) = v

isInclude :: Chunk -> Bool
isInclude (Include _) = True
isInclude (Exclude _) = False

chunkValueV :: ChunkV -> Bool
chunkValueV (Includev v) = v
chunkValueV (Excludev v) = v

isIncludeV :: ChunkV -> Bool
isIncludeV (Includev _) = True
isIncludeV (Excludev _) = False


main :: IO ()
main = do 
    text <- readFile "advent07.txt" 
    part1 text
    part2 text


part1 :: String -> IO ()
part1 text = do 
    print $ length $ filter (allowsAbba) $ successfulParse $ parseI7vf text


part2 :: String -> IO ()
part2 text = do 
    print $ length $ filter (supportsSSL) $ successfulParse $ parseI7f text


allowsAbba :: [ChunkV] -> Bool
allowsAbba chunks = (any (chunkValueV) includeChunks) && (not (any (chunkValueV) excludeChunks))
    where (includeChunks, excludeChunks) = partition (isIncludeV) chunks


i7file = i7line `endBy` newline 
i7line = many1 (includeChunk <|> excludeChunk)

chunk = many1 alphaNum

excludeChunk = Exclude <$> (between (char '[') (char ']') $ chunk)
includeChunk = Include <$> chunk

hasABBA = preambleAbba <* (many alphaNum)
preambleAbba = (try abba) <|> (alphaNum >> preambleAbba)

abba = 
    do  a <- alphaNum
        b <- alphaNum
        if a == b then
            fail "Identical"
        else do char b
                char a
                return [a, b, b, a]

i7filev = i7linev `endBy` newline
i7linev = many1 (includeChunkv <|> excludeChunkv)

excludeChunkv = Excludev <$> (between (char '[') (char ']') $ hasABBAv)
includeChunkv = Includev <$> hasABBAv

hasABBAv = 
    (try (id True <$ preambleAbba <* (many alphaNum)))
    <|>
    (id False <$ (many1 alphaNum))


parseI7f :: String -> Either ParseError [[Chunk]]
parseI7f input = parse i7file "(unknown)" input

parseI7 :: String -> Either ParseError [Chunk]
parseI7 input = parse i7line "(unknown)" input

parseAbba :: String -> Either ParseError String
parseAbba input = parse hasABBA "(unknown)" input

parseI7v :: String -> Either ParseError [ChunkV]
parseI7v input = parse i7linev "(unknown)" input

parseI7vf :: String -> Either ParseError [[ChunkV]]
parseI7vf input = parse i7filev "(unknown)" input

successfulParse :: Either ParseError [a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = a


allSubstrings :: Int -> [a] -> [[a]]
allSubstrings n es 
    | length es < n = []
    | otherwise = (take n es) : (allSubstrings n $ tail es)

ieCandidates :: [Chunk] -> ([String], [String])
ieCandidates chunks = (includeCandidates, excludeCandidates)
    where (includeChunks, excludeChunks) = partition (isInclude) chunks
          isABA s = (s!!0 == s!!2) && (s!!0 /= s!!1)
          candidates = (filter (isABA)) . (foldl (union) []) . (map ((allSubstrings 3) . chunkValue))
          includeCandidates = candidates includeChunks
          excludeCandidates = candidates excludeChunks
          
inverseABA :: String -> String
inverseABA s = [s!!1, s!!0, s!!1]

supportsSSL :: [Chunk] -> Bool
supportsSSL chunks = not $ null $ intersect abas eabas
    where (abas, babs) = ieCandidates chunks
          eabas = map (inverseABA) babs
