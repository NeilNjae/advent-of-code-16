import Text.Parsec hiding (State)
import Data.Text (pack, unpack, toTitle)
import Control.Monad.State.Lazy
import Data.List (partition, findIndices, sort, find)
import Data.Maybe (fromJust)

data Destination = Bot | Output deriving (Show, Read, Eq)
-- Rule bot low-destination high-destination
-- Gift bot value
data Instruction = Rule { ruleId :: Int 
                        , lowDestType :: Destination
                        , lowDestId :: Int
                        , highDestType :: Destination
                        , highDestId :: Int
                        } | 
                   Gift { giftId :: Int
                        , value :: Int 
                        }
                   deriving (Show)

-- bod id [item1, item2]
data Place = Place { placeId :: Int
                   , placeType :: Destination
                   , items :: [Int]} 
                   deriving (Show)

-- delivery by bot of low-value and high-value
data Event = Delivery { deliveryId :: Int
                      , lowDelivery :: Int
                      , highDelivery :: Int
                      } | 
             Update { updateId :: Int
                    , updateType :: Destination
                    , updateItem :: Int
                    } deriving (Show)

type Factory = ([Place], [Instruction], [Event]) 
-- data FactorySt History = FactorySt (Factory -> (Factory, History))

emptyFactory :: Factory
emptyFactory = ([], [], [])

main :: IO ()
main = do 
    text <- readFile "data/advent10.txt" 
    let instructions = successfulParse $ parseIfile text
    part1 instructions
    part2 instructions


part1 :: [Instruction] -> IO ()
part1 instructions = 
    do  let (_, _, events) = snd $  runState (runFactory instructions) emptyFactory
        -- let (places, instructions, events) = snd finalFactory
        print $ deliveryId $ fromJust $ findDelivery events 17 61

part2 :: [Instruction] -> IO ()
part2 instructions = 
    do  let (places, _, _) = snd $  runState (runFactory instructions) emptyFactory
        let outs = findOutputs places [0, 1, 2]
        let product = foldl1 (*) $ concatMap (items) outs
        print $ product


findDelivery :: [Event] -> Int -> Int -> Maybe Event
findDelivery events lowItem highItem = find (delivery) events
    where delivery Update {} = False
          delivery Delivery {deliveryId = bot, lowDelivery = l, highDelivery = h} 
            | l == lowItem && h == highItem = True
            | otherwise = False

findOutputs :: [Place] -> [Int] -> [Place]
findOutputs outputs ids = filter (interesting) outputs
    where interesting Place {placeId = p, placeType = t, items = i}
            | (p `elem` ids) && t == Output = True
            | otherwise = False


runFactory :: [Instruction] -> State Factory ()
runFactory instructions = do
    addInstructions instructions
    runInstructions instructions



instructionFile = instructionLine `endBy` newline 
instructionLine = ruleL <|> giftL


ruleL = 
    do (string "bot" >> spaces)
       bot <- many1 digit
       (spaces >> string "gives low to" >> spaces)
       lowDestType <- (string "output" <|> string "bot")
       spaces
       lowDest <- many1 digit
       (spaces >> string "and high to" >> spaces)
       highDestType <- (string "output" <|> string "bot")
       spaces
       highDest <- many1 digit
       let rule = Rule (read bot)
                   (read $ unpack $ toTitle $ pack lowDestType)
                   (read lowDest)
                   (read $ unpack $ toTitle $ pack highDestType)
                   (read highDest)
       return rule

giftL = 
    do (string "value" >> spaces)
       value <- many1 digit
       (spaces >> string "goes to bot" >> spaces)
       bot <- many1 digit
       let gift = Gift (read bot) (read value)
       return gift


parseIfile :: String -> Either ParseError [Instruction]
parseIfile input = parse instructionFile "(unknown)" input

parseIline :: String -> Either ParseError Instruction
parseIline input = parse instructionLine "(unknown)" input

successfulParse :: Either ParseError [a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = a




addInstructions :: [Instruction] -> State Factory ()
addInstructions [] = return ()
addInstructions (i:is) = do
    addInstruction i
    addInstructions is


addInstruction :: Instruction -> State Factory ()
addInstruction r@(Rule {lowDestType = ld, lowDestId = li,
                     highDestType = hd, highDestId = hi}) = 
    do (places, rules, history) <- get
       put (places, r:rules, history)
       addPlace (Place {placeType = ld, placeId = li, items = []})
       addPlace (Place {placeType = hd, placeId = hi, items = []})
addInstruction Gift {giftId = g} = 
    do addPlace (Place {placeType = Bot, placeId = g, items = []})


addPlace :: Place -> State Factory ()
addPlace place = 
    do  (places, rules, history) <- get
        if not $ placeElem place places
            then put ((place:places), rules, history)
        else return ()


runInstructions :: [Instruction] -> State Factory ()
runInstructions [] = return ()
runInstructions (i:is) = 
    do  runInstruction i
        runInstructions is


runInstruction :: Instruction -> State Factory ()
runInstruction Rule {} = return ()
runInstruction g@(Gift {}) = 
    do  updatePlace (giftId g) Bot (value g)
        propogateUpdates

updatePlace :: Int -> Destination -> Int -> State Factory ()
updatePlace b d i = 
    do  (places, instructions, events) <- get
        let (place0s, otherPlaces) = partition (samePlace (Place {placeId = b, placeType = d, items = []})) places
        let place = head place0s
        let place' = place {items = i:(items place)}
        let update = Update {updateId = b, updateType = d, updateItem = i}
        put (place':otherPlaces, instructions, update:events)
  

propogateUpdates :: State Factory ()
propogateUpdates = 
    do  (places, instructions, events) <- get
        let (fullBots, otherPlaces) = fullRobots places
        if (not . null) fullBots
            then do let fullBot = head fullBots
                    let maybeRule = findRule instructions (placeId fullBot)
                    case maybeRule of
                        Nothing -> propogateUpdates
                        Just rule -> do let small:large:_ = sort $ items fullBot
                                        let emptyBot = fullBot {items = []}
                                        let delivery = Delivery { deliveryId = placeId fullBot
                                                                , lowDelivery = small
                                                                , highDelivery = large
                                                                }
                                        put (emptyBot:(tail fullBots) ++ otherPlaces,
                                             instructions, 
                                             delivery:events)
                                        updatePlace (lowDestId rule) (lowDestType rule) small
                                        updatePlace (highDestId rule) (highDestType rule) large
                                        propogateUpdates
            else return ()


placeElem :: Place -> [Place] -> Bool
placeElem place places = (not . null) $ findIndices (samePlace place) places

samePlace :: Place -> Place -> Bool
samePlace p1 p2 = (placeId p1 == placeId p2) && (placeType p1 == placeType p2)

fullRobots :: [Place] -> ([Place], [Place])
fullRobots places = partition (\p -> placeType p == Bot && length (items p) >= 2) places

findRule :: [Instruction] -> Int -> Maybe Instruction
findRule instructions bot = find ruleForBot instructions
    where ruleForBot Gift {} = False
          ruleForBot Rule {ruleId = b}
            | b == bot = True
            | otherwise = False
