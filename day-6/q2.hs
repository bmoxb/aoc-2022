import Data.HashMap.Strict

main = do
    s <- readFile "input.txt"
    print (findMarker s)

findMarker :: String -> Int
findMarker s =
    search s 14 (fromListWith (+) [(x, 1) | x <- take 14 s])
    where
        search (x:xs) index map
            | (length map == 14) = index
            | otherwise = search xs (index + 1) (removeAdd x (xs !! 13) map)

removeAdd :: Char -> Char -> HashMap Char Int -> HashMap Char Int
removeAdd remove add map =
    update decOrNothing remove (insertWith (+) add 1 map)
    where decOrNothing x
                     | x <= 1 = Nothing
                     | otherwise = Just (x - 1)
