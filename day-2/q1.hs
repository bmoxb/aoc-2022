main = do
    s <- readFile "input.txt"
    print (sum (evalGuide (makeGuide s)))

type StrategyGuide = [(Move, Move)]

data Move = Rock | Paper | Scissors deriving (Eq)

evalGuide :: StrategyGuide -> [Int]
evalGuide guide = [movePairFinalScore pair | pair <- guide]

makeGuide :: String -> StrategyGuide
makeGuide s =
    [lineToMovePair line | line <- (lines s)]
    where lineToMovePair line = (charToMove (line !! 0), charToMove (line !! 2))

charToMove :: Char -> Move
charToMove c
    | elem c "AX" = Rock
    | elem c "BY" = Paper
    | elem c "CZ" = Scissors

movePairFinalScore :: (Move, Move) -> Int
movePairFinalScore pair =
    (individualMoveScore (snd pair)) + (movePairPlayScore pair)

movePairPlayScore :: (Move, Move) -> Int
movePairPlayScore (l, r)
    | elem (l, r) winningPairs = 6
    | l == r                   = 3
    | otherwise                = 0
    where winningPairs = [(Rock, Paper), (Paper, Scissors), (Scissors, Rock)]

individualMoveScore :: Move -> Int
individualMoveScore m =
    case m of Rock -> 1
              Paper -> 2
              Scissors -> 3

