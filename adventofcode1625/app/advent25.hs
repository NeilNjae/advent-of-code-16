module Main(main) where

import Text.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number
import Control.Monad.State.Lazy

-- import Control.Monad.Writer
import Control.Monad.Reader
import Debug.Trace

data Location = Literal Int | Register Char deriving (Show, Eq)
data Instruction =   Cpy Location Location 
                   | Inc Location 
                   | Dec Location 
                   | Jnz Location Location
                   | Tgl Location
                   | Out Location
                   deriving (Show, Eq)

data Machine = Machine { a :: Int
                       , b :: Int
                       , c :: Int
                       , d :: Int
                       , pc :: Int
                       , instructions :: [Instruction]
                       , execCount :: Int
                       } 
               deriving (Show, Eq)

data AppConfig = AppConfig { cfgMaxRun :: Int } deriving (Show)


type App = ReaderT AppConfig (State Machine) String


testInstructions1 = "\
\cpy 5 d\n\
\cpy 7 c\n\
\cpy 362 b\n\
\inc d\n\
\dec b\n\
\jnz b -2\n\
\dec c\n\
\jnz c -5\n\
\out d"

testInstructions2 = "jnz 1 0"

target :: String
target = cycle "01"

emptyMachine :: Machine
emptyMachine = Machine {a=0, b=0, c=0, d=0, pc=0, instructions=[], execCount=0}

main :: IO ()
main = do 
    text <- readFile "data/advent25.txt" 
    -- let text = testInstructions1
    let instructions = successfulParse $ parseIfile text
    part1 instructions


part1 :: [Instruction] -> IO ()
part1 instrs = 
    print $ head validInputs
    where m0 = emptyMachine {instructions=instrs}
          inputs = [0..]
          validInputs = filter (validMachine) inputs
          validMachine i = valid $ evalMachine m0 i


valid :: String -> Bool
valid output = all (\p -> fst p == snd p) $ zip target output

evalMachine :: Machine -> Int -> String
evalMachine machine0 input = evalState (runReaderT (runMachine "") config) m
    where m = machine0 {a = input}
          config = AppConfig {cfgMaxRun = 500000}

runMachine :: String -> App
runMachine output = 
    do  cfg <- ask
        m <- get
        if (pc m) >= (length $ instructions m) || execCount m > cfgMaxRun cfg
            then return output
            else do thisOutput <- executeStep
                    runMachine (output ++ thisOutput)


executeStep :: App
executeStep = 
    do  m <- get
        let i = (instructions m)!!(pc m)
        let output = generateOutput i m
        put (executeInstructionPeep i m) {execCount = (execCount m) + 1}
        -- put (executeInstruction i m) {execCount = (execCount m) + 1}
        return output

generateOutput :: Instruction -> Machine -> String
generateOutput (Out a) m = show $ evaluate m a
generateOutput _ _ = ""

executeInstructionPeep :: Instruction -> Machine -> Machine
executeInstructionPeep i m =
    if sample == sampleTarget
        -- then trace ("Peeping 1 " ++ (show m) ++ " to " ++ (show m1)) m1
        then m1
        else executeInstruction i m
    where sample = take (length sampleTarget) $ drop (pc m) $ instructions m 
          sampleTarget = [  Inc (Register 'd')
                          , Dec (Register 'b')
                          , Jnz (Register 'b') (Literal (-2))
                          , Dec (Register 'c')
                          , Jnz (Register 'c') (Literal (-5)) ]
          m1 = m {d = d m + c m * b m, c = 0, b = 0, pc = pc m + (length sample)}


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
executeInstruction (Out _) m = m {pc = pc m + 1}


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
instructionLine = incL <|> decL <|> cpyL <|> jnzL <|> tglL <|> outL

incL = Inc <$> (string "inc" *> spaces *> register)
decL = Dec <$> (string "dec" *> spaces *> register)
cpyL = Cpy <$> (string "cpy" *> spaces *> location) <*> (spaces *> register)
jnzL = Jnz <$> (string "jnz" *> spaces *> location) <*> (spaces *> location)
tglL = Tgl <$> (string "tgl" *> spaces *> location)
outL = Out <$> (string "out" *> spaces *> location)

location = (Literal <$> int) <|> register
register = Register <$> (oneOf "abcd")

parseIfile :: String -> Either ParseError [Instruction]
parseIfile input = parse instructionFile "(unknown)" input

parseIline :: String -> Either ParseError Instruction
parseIline input = parse instructionLine "(unknown)" input

successfulParse :: Either ParseError [a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = a
