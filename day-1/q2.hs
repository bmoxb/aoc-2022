import qualified Data.Heap as Heap

main = do
    s <- readFile "input.txt"
    print (sum (topThree s))

topThree :: String -> [Int]
topThree s =
    Heap.take 3 heap
    where heap = orderedCalories (lines s) 0 Heap.empty

orderedCalories :: [String] -> Int -> Heap.MaxHeap Int -> Heap.MaxHeap Int

orderedCalories (x:xs) currentSum results =
    case x of "" -> orderedCalories xs 0 (Heap.insert currentSum results)
              val -> orderedCalories xs (currentSum + (read val)) results

orderedCalories [] currentSum results = Heap.insert currentSum results
