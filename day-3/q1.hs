import Data.HashSet
import Data.Char (ord)

main = do
    s <- readFile "input.txt"
    let priorities = [itemToPriority (identifyCommonItem sack) | sack <- collectRucksacks s]
    print (sum priorities)

type Rucksack = (String, String)

collectRucksacks :: String -> [Rucksack]
collectRucksacks s =
    [splitAt ((length line) `div` 2) line | line <- lines s]

identifyCommonItem :: Rucksack -> Char
identifyCommonItem (left, right) =
    head (toList set)
    where set = intersection (fromList left) (fromList right)

itemToPriority :: Char -> Int
itemToPriority c
    | 'a' <= c && c <= 'z' = (ord c) - (ord 'a') + 1
    | 'A' <= c && c <= 'Z' = (ord c) - (ord 'A') + 27
