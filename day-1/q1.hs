main = do
    s <- readFile "input.txt"
    print (mostCalories (lines s) 0 0)

mostCalories :: [String] -> Int -> Int -> Int

mostCalories (x:xs) currentSum best =
    case x of "" -> mostCalories xs 0 (max currentSum best)
              val -> mostCalories xs (currentSum + (read val)) best

mostCalories [] currentSum best = max currentSum best
