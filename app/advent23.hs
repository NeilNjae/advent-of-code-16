module Main(main) where

import Text.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number
import Control.Monad.State.Lazy
-- import Debug.Trace

data Location = Literal Int | Register Char deriving (Show, Eq)
data Instruction =   Cpy Location Location 
                   | Inc Location 
                   | Dec Location 
                   | Jnz Location Location
                   | Tgl Location
                   deriving (Show, Eq)

data Machine = Machine { a :: Int
                       , b :: Int
                       , c :: Int
                       , d :: Int
                       , pc :: Int
                       , instructions :: [Instruction]} 
               deriving (Show, Eq)

testInstructions = "cpy 2 a\n\
\tgl a\n\
\tgl a\n\
\tgl a\n\
\cpy 1 a\n\
\dec a\n\
\dec a"

emptyMachine :: Machine
emptyMachine = Machine {a=0, b=0, c=0, d=0, pc=0, instructions=[]}

main :: IO ()
main = do 
    text <- readFile "data/advent23.txt" 
    let instructions = successfulParse $ parseIfile text
    part1 instructions
    part2 instructions


part1 :: [Instruction] -> IO ()
part1 instrs = 
    do  let m0 = emptyMachine {instructions=instrs, a = 7}
        let mf = snd $ runState runMachine m0
        print (a mf)

part2 :: [Instruction] -> IO ()
part2 instrs = 
    do  let m0 = emptyMachine {instructions=instrs, a = 12}
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
        put (executeInstructionPeep i m)
        -- put (executeInstruction i m)

executeInstructionPeep :: Instruction -> Machine -> Machine
executeInstructionPeep i m =
    if sample1 == sample1Target
        -- then trace ("Peeping 1 " ++ (show m) ++ " to " ++ (show m1)) m1
        then m1
        else if sample2 == sample2Target
            -- then trace ("Peeping 2 " ++ (show m) ++ " to " ++ (show m2)) m2
            then m2
            else executeInstruction i m
    where sample1 = take (length sample1Target) $ drop (pc m) $ instructions m 
          sample1Target = [ Cpy (Literal 0)    (Register 'a')
                          , Cpy (Register 'b') (Register 'c')
                          , Inc (Register 'a')
                          , Dec (Register 'c')
                          , Jnz (Register 'c') (Literal (-2))
                          , Dec (Register 'd')
                          , Jnz (Register 'd') (Literal (-5)) ]
          m1 = m {a = b m * d m, c = 0, d = 0, pc = pc m + (length sample1)}
          sample2 = take (length sample2Target) $ drop (pc m) $ instructions m 
          sample2Target = [ Dec (Register 'b')
                          , Cpy (Register 'b') (Register 'c')
                          , Cpy (Register 'c') (Register 'd')
                          , Dec (Register 'd')
                          , Inc (Register 'c')
                          , Jnz (Register 'd') (Literal (-2)) ]
          m2 = m {b = b m - 1, c = (b m - 1) * 2, d = 0, pc = pc m + (length sample2)}


executeInstruction :: Instruction -> Machine -> Machine
executeInstruction (Inc r@(Register _)) m = m' {pc=pc1}
    where pc1 = (pc m) + 1
          v = evaluate m r
          m' = writeValue m r (v+1)
executeInstruction (Inc (Literal _)) m = m {pc=pc1}
    where pc1 = (pc m) + 1
executeInstruction (Dec r@(Register _)) m = m' {pc=pc1}
    where pc1 = (pc m) + 1
          v = evaluate m r
          m' = writeValue m r (v-1)
executeInstruction (Dec (Literal _)) m = m {pc=pc1}
    where pc1 = (pc m) + 1
executeInstruction (Cpy s d@(Register _)) m = m' {pc=pc1}
    where pc1 = (pc m) + 1
          v = evaluate m s
          m' = writeValue m d v
executeInstruction (Cpy s (Literal _)) m = m {pc=pc1}
    where pc1 = (pc m) + 1
executeInstruction (Jnz s d) m 
    | v == 0 = m {pc=pc1}
    | otherwise = m {pc=pcj}
    where pc1 = (pc m) + 1
          ed = evaluate m d
          pcj = (pc m) + ed
          v = evaluate m s
executeInstruction (Tgl a) m 
    | v < (length $ instructions m) = m {instructions = (replace (instructions m) i' v),
                                       pc=pc1}
    | otherwise = m {pc=pc1}
    where pc1 = pc m + 1
          v = evaluate m a + pc m
          i = (instructions m)!!v
          i' = case i of
                  Inc x -> Dec x
                  Dec x -> Inc x
                  Tgl x -> Inc x
                  Cpy x y -> Jnz x y
                  Jnz x y -> Cpy x y
          replace xs x i = take i xs ++ [x] ++ drop (i+1) xs


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


instructionFile = instructionLine `sepEndBy` newline 
instructionLine = incL <|> decL <|> cpyL <|> jnzL <|> tglL

incL = Inc <$> (string "inc" *> spaces *> register)
decL = Dec <$> (string "dec" *> spaces *> register)
cpyL = Cpy <$> (string "cpy" *> spaces *> location) <*> (spaces *> register)
jnzL = Jnz <$> (string "jnz" *> spaces *> location) <*> (spaces *> location)
tglL = Tgl <$> (string "tgl" *> spaces *> location)

location = (Literal <$> int) <|> register
register = Register <$> (oneOf "abcd")

parseIfile :: String -> Either ParseError [Instruction]
parseIfile input = parse instructionFile "(unknown)" input

parseIline :: String -> Either ParseError Instruction
parseIline input = parse instructionLine "(unknown)" input

successfulParse :: Either ParseError [a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = a
