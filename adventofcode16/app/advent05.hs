module Main(main) where

import Data.Hash.MD5 (md5s, Str(..))
import Data.List (isPrefixOf)
import qualified Data.Map.Lazy as Map 

type Password = Map.Map Integer Char

input = "cxdnnyjw"

main :: IO ()
main = do 
        part1 
        part2


part1 :: IO ()
part1 = do 
    putStrLn $ take 8 [h!!5 | h <- filter (interesting) $ md5sequence input 0]

part2 :: IO ()
part2 = do 
    putStrLn $ Map.foldr (:) [] password
    where interestingHashes = 
            [(read [h!!5], h!!6) | 
              h <- filter (interesting) (md5sequence input 0), 
              h!!5 `elem` "01234567"]
          password = findPassword Map.empty interestingHashes


md5sequence :: String -> Integer -> [String]
md5sequence key i = (md5s (Str (key ++ show i))) : (md5sequence key (i+1))

interesting :: String -> Bool
interesting hash = "00000" `isPrefixOf` hash

dontReplace :: (Integer, Char) -> Password -> Password
dontReplace (k, v) = Map.insertWith (\_ v -> v) k v

findPassword :: Password -> [(Integer, Char)] -> Password
findPassword p (c:cs)
  | Map.size p == 8 = p
  | otherwise = findPassword p' cs
      where p' = dontReplace c p
