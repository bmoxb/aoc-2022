import Data.List.Split (splitOn)

main = do
    s <- readFile "input.txt"
    print (length (filter doesFullyContain (collectSectionPairs s)))

type SectionPair = (Section, Section)
type Section = (Int, Int)

collectSectionPairs :: String -> [SectionPair]
collectSectionPairs s = map makeSectionPair (lines s)

makeSectionPair :: String -> SectionPair
makeSectionPair line =
    (makeSection (split !! 0), makeSection (split !! 1))
    where split = splitOn "," line

makeSection :: String -> Section
makeSection s =
    (read (split !! 0), read (split !! 1))
    where split = splitOn "-" s

doesFullyContain :: SectionPair -> Bool
doesFullyContain ((x0, x1), (y0, y1)) =
    (x0 <= y0 && x1 >= y1) || (y0 <= x0 && y1 >= x1)
