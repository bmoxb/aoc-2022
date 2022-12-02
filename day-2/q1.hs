main = do
    s <- readFile "input.txt"
    print (calculateTotalScore s)

calculateTotalScore :: String -> Int
calculateTotalScore s =
    sum scores
    where pairs = identifyPairs (lines s)
          scores = [finalScoreFromPair p | p <- pairs]

identifyPairs :: [String] -> [(Move, Move)]
identifyPairs (x:xs) = [identifyPair x] ++ (identifyPairs xs)
identifyPairs [] = []

identifyPair :: String -> (Move, Move)
identifyPair s = (moveFromChar (s !! 0), moveFromChar (s !! 2))

data Move = Rock | Paper | Scissors deriving (Eq)

moveFromChar :: Char -> Move
moveFromChar c
    | elem c "AX" = Rock
    | elem c "BY" = Paper
    | elem c "CZ" = Scissors

scoreFromMove :: Move -> Int
scoreFromMove m = case m of Rock -> 1
                            Paper -> 2
                            Scissors -> 3

finalScoreFromPair :: (Move, Move) -> Int
finalScoreFromPair (l, r) = (scoreFromMove r) + (outcomeScoreFromPair (l, r))

outcomeScoreFromPair :: (Move, Move) -> Int
outcomeScoreFromPair (l, r)
    | elem (l, r) winningPairs = 6
    | l == r                   = 3
    | otherwise                = 0
    where winningPairs = [(Rock, Paper), (Paper, Scissors), (Scissors, Rock)]
