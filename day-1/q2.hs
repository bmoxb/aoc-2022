import Data.Heap

main = do
    s <- readFile "input.txt"
    print (sum (topThree s))

topThree :: String -> [Int]
topThree s =
    Data.Heap.take 3 heap 
    where heap = mostCalories (lines s) 0 empty

mostCalories :: [String] -> Int -> MaxHeap Int -> MaxHeap Int

mostCalories (x:xs) currentSum results =
    case x of "" -> mostCalories xs 0 (insert currentSum results)
              val -> mostCalories xs (currentSum + (read val)) results

mostCalories [] currentSum results = insert currentSum results
