main = do
    s <- readFile "input.txt"
    print (calculateTotalScore s)

calculateTotalScore :: String -> Int
calculateTotalScore s =
    sum scores
    where pairs = identifyPairs (lines s)
          scores = [finalScoreFromPair pair | pair <- pairs]

identifyPairs :: [String] -> [(Move, Outcome)]
identifyPairs (x:xs) = [identifyPair x] ++ (identifyPairs xs)
identifyPairs [] = []

identifyPair :: String -> (Move, Outcome)
identifyPair s = (moveFromChar (s !! 0), outcomeFromChar (s !! 2))

data Move = Rock | Paper | Scissors deriving (Eq)

moveFromChar :: Char -> Move
moveFromChar c = case c of 'A' -> Rock
                           'B' -> Paper
                           'C' -> Scissors

scoreFromMove :: Move -> Int
scoreFromMove m = case m of Rock -> 1
                            Paper -> 2
                            Scissors -> 3

data Outcome = Win | Draw | Lose

outcomeFromChar :: Char -> Outcome
outcomeFromChar c = case c of 'X' -> Lose
                              'Y' -> Draw
                              'Z' -> Win

finalScoreFromPair :: (Move, Outcome) -> Int
finalScoreFromPair (m, o) =
    (scoreFromMove myMove) + (outcomeScoreFromMoves (m, myMove))
    where myMove = moveGivenDesiredOutcome m o

outcomeScoreFromMoves :: (Move, Move) -> Int
outcomeScoreFromMoves (l, r)
    | elem (l, r) winningPairs = 6
    | l == r                   = 3
    | otherwise                = 0
    where winningPairs = [(Rock, Paper), (Paper, Scissors), (Scissors, Rock)]

moveGivenDesiredOutcome :: Move -> Outcome -> Move
moveGivenDesiredOutcome move outcome =
    case outcome of Draw -> move
                    Win -> winningMoveAgainst move
                    Lose -> losingMoveAgainst move

winningMoveAgainst :: Move -> Move
winningMoveAgainst m = case m of Rock -> Paper
                                 Paper -> Scissors
                                 Scissors -> Rock

losingMoveAgainst :: Move -> Move
losingMoveAgainst m = case m of Rock -> Scissors
                                Paper -> Rock
                                Scissors -> Paper
