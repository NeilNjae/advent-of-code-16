module Main(main) where

import Text.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number
import Data.List (partition, union, intersect, tails)
import Data.Char (isDigit)
import Control.Monad.State.Lazy

data Location = Literal Int | Register Char deriving (Show)
data Instruction = Cpy Location Location | 
                   Inc Location |
                   Dec Location | 
                   Jnz Location Int
                   deriving (Show)

data Machine = Machine { a :: Int
                       , b :: Int
                       , c :: Int
                       , d :: Int
                       , pc :: Int
                       , instructions :: [Instruction]} 
               deriving (Show)

emptyMachine :: Machine
emptyMachine = Machine {a=0, b=0, c=0, d=0, pc=0, instructions=[]}

main :: IO ()
main = do 
    text <- readFile "data/advent12.txt" 
    let instructions = successfulParse $ parseIfile text
    part1 instructions
    part2 instructions


part1 :: [Instruction] -> IO ()
part1 instrs = 
    do  let m0 = emptyMachine {instructions=instrs}
        let mf = snd $ runState runMachine m0
        print (a mf)

part2 :: [Instruction] -> IO ()
part2 instrs = 
    do  let m0 = emptyMachine {instructions=instrs, c=1}
        let mf = snd $ runState runMachine m0
        print (a mf)



runMachine :: State Machine ()
runMachine = 
    do  m <- get
        if (pc m) >= (length $ instructions m)
            then return ()
            else do executeStep
                    runMachine

executeStep :: State Machine ()
executeStep = 
    do  m <- get
        let i = (instructions m)!!(pc m)
        put (executeInstruction i m)

executeInstruction :: Instruction -> Machine -> Machine
executeInstruction (Inc (Register r)) m = m' {pc=pc1}
    where pc1 = (pc m) + 1
          v = evaluate m (Register r)
          m' = writeValue m (Register r) (v+1)
executeInstruction (Dec (Register r)) m = m' {pc=pc1}
    where pc1 = (pc m) + 1
          v = evaluate m (Register r)
          m' = writeValue m (Register r) (v-1)
executeInstruction (Cpy s d) m = m' {pc=pc1}
    where pc1 = (pc m) + 1
          v = evaluate m s
          m' = writeValue m d v
executeInstruction (Jnz s d) m 
    | v == 0 = m {pc=pc1}
    | otherwise = m {pc=pcj}
    where pc1 = (pc m) + 1
          pcj = (pc m) + d
          v = evaluate m s
          

evaluate :: Machine -> Location -> Int
evaluate _ (Literal i) = i
evaluate m (Register r) = 
    case r of
        'a' -> (a m)
        'b' -> (b m)
        'c' -> (c m)
        'd' -> (d m)

writeValue :: Machine -> Location -> Int -> Machine
writeValue m (Literal i) _ = m
writeValue m (Register r) v =
    case r of 
        'a' -> m {a=v}
        'b' -> m {b=v}
        'c' -> m {c=v}
        'd' -> m {d=v}


instructionFile = instructionLine `endBy` newline 
-- instructionLine = choice [cpyL, incL, decL, jnzL]
instructionLine = incL <|> decL <|> cpyL <|> jnzL

incL = incify <$> (string "inc" *> spaces *> (oneOf "abcd"))
        where incify r = Inc (Register r)
decL = decify <$> (string "dec" *> spaces *> (oneOf "abcd"))
        where decify r = Dec (Register r)
cpyL = cpyify <$> (string "cpy" *> spaces *> ((many1 letter) <|> (many1 digit))) 
                  <*> (spaces *> (oneOf "abcd"))
        where cpyify s r = Cpy (readLocation s) (Register r)
jnzL = jnzify <$> (string "jnz" *> spaces *> ((many1 letter) <|> (many1 digit))) 
                  <*> (spaces *> int)
        where jnzify r d = Jnz (readLocation r) d


readLocation :: String -> Location
readLocation l
    | all (isDigit) l = Literal (read l)
    | otherwise = Register (head l)



parseIfile :: String -> Either ParseError [Instruction]
parseIfile input = parse instructionFile "(unknown)" input

parseIline :: String -> Either ParseError Instruction
parseIline input = parse instructionLine "(unknown)" input

successfulParse :: Either ParseError [a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = a