import Text.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number
-- import Control.Applicative ((<*), (*>), (<*>))
import Data.Maybe (fromJust)
import Data.List (elemIndex)

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

data Instruction =    SwapPosition Int Int 
                    | SwapLetter Char Char
                    | RotateSteps Int
                    | RotateLetter Char
                    | Reverse Int Int
                    | Move Int Int 
                    deriving (Show, Eq)

data Log = Log {
    action :: String
    } deriving (Show)

data Password = Password {
    password :: String
    } deriving (Show)


type App = WriterT [Log] (StateT Password Identity)

infixl 9 ??

(??) :: Eq a => [a] -> a -> Int
(??) items item = fromJust $ elemIndex item items


initial = "abcdefgh"
final   = "fbgdceah"

testInstructions = "\
\swap position 4 with position 0\n\
\swap letter d with letter b\n\
\reverse positions 0 through 4\n\
\rotate left 1 step\n\
\move position 1 to position 4\n\
\move position 3 to position 0\n\
\rotate based on position of letter b\n\
\rotate based on position of letter d\n"

main :: IO ()
main = do 
    -- let ti = successfulParse $ parseIfile testInstructions
    -- part1 ti "abcde"
    -- part2 (reverse ti) "decab"
    text <- readFile "advent21.txt" 
    let instructions = successfulParse $ parseIfile text
    part1 instructions initial
    part2 (reverse instructions) final

part1 :: [Instruction] -> String -> IO ()
part1 instructions start = 
    let state = Password {password = start}
    in print $ runIdentity (runStateT (runWriterT (apply instructions)) state)
    -- in putStrLn $ password $ runIdentity (execStateT (runWriterT (apply instructions)) state)

part2 :: [Instruction] -> String -> IO ()
part2 instructions end = 
    let state = Password {password = end}
    in print $ runIdentity (runStateT (runWriterT (unApply instructions)) state)
    -- in putStrLn $ password $ runIdentity (execStateT (runWriterT (apply instructions)) state)


apply :: [Instruction] -> App ()
apply [] = return ()
apply (i:is) = 
    do  st <- get
        let p0 = password st
        let p1 = applyInstruction i p0
        put st {password = p1}
        tell [Log (p0 ++ " -> " ++ p1 ++ " : " ++ (show i))]
        apply is


applyInstruction :: Instruction -> String -> String
applyInstruction (SwapPosition from to) p0 
    | from == to = p0
    | otherwise = prefix ++ [p0!!end] ++ midfix ++ [p0!!start] ++ suffix
        where start = minimum [from, to]
              end = maximum [from, to]
              prefix = take start p0
              midfix = take (end-start-1) $ drop (start+1) p0
              suffix = drop (end+1) p0

applyInstruction (SwapLetter l0 l1) p0 = applyInstruction (SwapPosition (p0??l0) (p0??l1)) p0

applyInstruction (RotateSteps n) p0 = (drop n' p0) ++ (take n' p0)
    where n' = if n < 0 
                then (-1 * n)
                else (length p0) - n

applyInstruction (RotateLetter l) p0 = applyInstruction (RotateSteps n) p0
    where n = (1 + (p0??l) + if (p0??l) >= 4 then 1 else 0) `mod` (length p0)

applyInstruction (Reverse from to) p0
    | from == to = p0
    | otherwise = prefix ++ (reverse midfix) ++ suffix
        where start = minimum [from, to]
              end = maximum [from, to]
              prefix = take start p0
              midfix = take (end-start+1) $ drop start p0
              suffix = drop (end+1) p0

applyInstruction (Move from to) p0 
    | from == to = p0
    | otherwise = prefix ++ [p0!!from] ++ suffix
        where without = take from p0 ++ drop (from+1) p0
              prefix = take to without
              suffix = drop (to) without


unApply :: [Instruction] -> App ()
unApply [] = return ()
unApply (i:is) = 
    do  st <- get
        let p0 = password st
        let p1 = unApplyInstruction i p0
        put st {password = p1}
        tell [Log (p1 ++ " <- " ++ p0 ++ " : " ++ (show i))]
        unApply is

unApplyInstruction :: Instruction -> String -> String
unApplyInstruction (SwapPosition from to) p0 = applyInstruction (SwapPosition from to) p0
unApplyInstruction (SwapLetter l0 l1) p0 = applyInstruction (SwapLetter l0 l1) p0
unApplyInstruction (RotateSteps n) p0 = applyInstruction (RotateSteps (-1 * n)) p0
unApplyInstruction (Reverse from to) p0 = applyInstruction (Reverse from to) p0
unApplyInstruction (Move from to) p0 = applyInstruction (Move to from) p0
unApplyInstruction (RotateLetter l) p0 = applyInstruction (RotateSteps n) p0
    where n = case (p0??l) of
                0 -> -1
                1 -> -1
                2 ->  2
                3 -> -2
                4 ->  1
                5 -> -3
                6 ->  0
                7 -> -4
    -- where n = case (p0??l) of
    --             0 -> -1
    --             1 -> -1
    --             2 ->  1
    --             3 -> -2
    --             4 ->  1


instructionFile = instructionLine `endBy` newline 
instructionLine = choice [ swapL 
                         , rotateL
                         , reverseL
                         , moveL
                         ]

swapL = (try (string "swap ")) *> (swapPosL <|> swapLetterL)

swapPosL = SwapPosition <$> (string "position" *> spaces *> int) 
                        <*> (spaces *> string "with position" *> spaces *> int)

swapLetterL = SwapLetter <$> (string "letter" *> spaces *> letter) 
                         <*> (spaces *> string "with letter" *> spaces *> letter)

rotateL = (try (string "rotate ")) *> (rotateDirL <|> rotateLetterL)

rotateDirL = rotateStepify <$> ((string "left") <|> (string "right"))
                           <*> (spaces *> int <* spaces <* skipMany letter)
    where rotateStepify dir n = case dir of 
                                     "left" -> (RotateSteps (-1 * n))
                                     "right" -> (RotateSteps n)
rotateLetterL = RotateLetter <$> (string "based on position of letter " *> letter)

reverseL = Reverse <$> (string "reverse positions" *> spaces *> int)
                   <*> (spaces *> (string "through") *> spaces *> int)

moveL = Move <$> (string "move position" *> spaces *> int)
             <*> (spaces *> (string "to position") *> spaces *> int)


parseIfile :: String -> Either ParseError [Instruction]
parseIfile input = parse instructionFile "(unknown)" input

parseIline :: String -> Either ParseError Instruction
parseIline input = parse instructionLine "(unknown)" input

successfulParse :: Either ParseError [a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = a
