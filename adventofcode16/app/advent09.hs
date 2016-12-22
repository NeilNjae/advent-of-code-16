module Main(main) where

import Data.List.Split (splitOn)
import Data.Char (isSpace)

type Chunk = (Int, String)

main :: IO ()
main = do 
        textL <- readFile "data/advent09.txt" 
        let text = filter (not . isSpace) textL
        part1 text
        part2 text

part1 :: String -> IO ()
part1 text = do 
    print $ cLength $ decompress text

part2 :: String -> IO ()
part2 text = do 
    print $ cLength $ decompress2 text


decompress :: String -> [Chunk]
decompress text = 
    if not (null msuf) 
        then (1, pre):(num, chunk):drest
        else [(1, pre)]
    where 
        (pre, msuf) = span ('(' /= ) text
        (marker, suf) = span (')' /= ) msuf
        ln = splitOn "x" (tail marker)
        len = read (ln!!0) :: Int
        num = read (ln!!1) :: Int
        (chunk, remainder) = splitAt len (tail suf)
        drest = decompress remainder

decompress2 :: String -> [Chunk]
decompress2 text = 
    if not (null msuf) 
        then [(1, pre)] ++ mulDchunks ++ drest
        else [(1, pre)]
    where 
        (pre, msuf) = span ('(' /= ) text
        (marker, suf) = span (')' /= ) msuf
        ln = splitOn "x" (tail marker)
        len = read (ln!!0) :: Int
        num = read (ln!!1) :: Int
        (chunk, remainder) = splitAt len (tail suf)
        dchunks = decompress2 chunk
        mulDchunks = [(dl * num, ds) | (dl, ds) <- dchunks]
        drest = decompress2 remainder

cLength :: [Chunk] -> Int
cLength = sum . map (clen)
    where clen (n, t) = n * (length t)

