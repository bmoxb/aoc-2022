import Data.HashMap.Strict

main = do
    s <- readFile "input.txt"
    print (findMarker s)

findMarker :: String -> Int
findMarker s =
    search s 4 (fromListWith (+) [(x, 1) | x <- take 4 s])
    where
        search s index map
            | (length map == 4) = index
            | otherwise = case s of
                               (a:b:c:d:e:tail) -> search (b:c:d:e:tail) (index + 1) (removeAdd a e map)
                               _ -> index

removeAdd :: Char -> Char -> HashMap Char Int -> HashMap Char Int
removeAdd remove add map =
    update decOrNothing remove (insertWith (+) add 1 map)
    where decOrNothing x
                     | x <= 1 = Nothing
                     | otherwise = Just (x - 1)
