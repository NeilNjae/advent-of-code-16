import Data.List (nub, tails)
import Data.ByteString.Char8 (pack)
import Crypto.Hash (hash, Digest, MD5)
import Control.Parallel.Strategies (parMap, rdeepseq)

salt = "yjdafjpo"
-- salt = "abc"

main :: IO ()
main = do 
        part1 
        part2

part1 :: IO ()
part1 = print $ head $ drop 63 $ filter (\i -> possibleKey sq i && confirmKey sq i) [0..]
    where sq = md5sequence

part2 :: IO ()
part2 = print $ head $ drop 63 $ filter (\i -> possibleKey sq i && confirmKey sq i) [0..]
    where sq = md5sequenceS

getHash :: String -> String
getHash bs = show (hash $ pack bs :: Digest MD5)

md5sequence :: [String]
-- md5sequence = [makeMd5 i | i <- [0..]]
md5sequence = parMap rdeepseq (makeMd5) [0..]
    where makeMd5 i = getHash (salt ++ show i)

md5sequenceS :: [String]
-- md5sequenceS = [makeMd5 i | i <- [0..]]
md5sequenceS = parMap rdeepseq (makeMd5) [0..]
    where makeMd5 i = stretch $ getHash (salt ++ show i)
          stretch h0 = foldr (\_ h -> getHash h) h0 [1..2016]

possibleKey :: [String] -> Int-> Bool
possibleKey s = not . null . repeats 3 . ((!!) s)

confirmKey :: [String] -> Int -> Bool
confirmKey s i = any (confirmation) $ take 1000 $ drop (i+1) s
    where c = head $ repeats 3 $ s!!i
          confirmation m = c `elem` (repeats 5 m)
                              
repeats :: Int -> String -> [String]
repeats n = filter (null . tail) . map (nub) . substrings n

substrings :: Int -> [a] -> [[a]]
substrings l = filter (\s -> (length s) == l) . map (take l) . tails
