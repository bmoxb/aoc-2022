import Data.HashSet
import Data.Char (ord)

main = do
    s <- readFile "input.txt"
    let priorities = [itemToPriority (identifyBadgeItem group) | group <- collectGroups 3 s]
    print (sum priorities)

type Group = [Rucksack]
type Rucksack = String

collectGroups :: Int -> String -> [Group]
collectGroups size s =
    collect (lines s)
    where collect ls
              | length ls > 3 = [(take size ls)] ++ (collect (drop size ls))
              | otherwise = [ls]

identifyBadgeItem :: Group -> Char
identifyBadgeItem group =
    head (toList (search group))
    where search [x] = fromList x
          search (x:xs) = intersection (fromList x) (search xs)

itemToPriority :: Char -> Int
itemToPriority c
    | 'a' <= c && c <= 'z' = (ord c) - (ord 'a') + 1
    | 'A' <= c && c <= 'Z' = (ord c) - (ord 'A') + 27
