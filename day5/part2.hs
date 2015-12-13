import Data.List

hasABA [] = False
hasABA [c1] = False
hasABA [c1,c2] = False
hasABA (c1:c2:c3:cs)
    | c1 == c3 = True
    | otherwise = hasABA (c2:c3:cs)

hasDoubles [] = False
hasDoubles [c1] = False
hasDoubles [c1,c2] = False
hasDoubles [c1,c2,c3] = False
hasDoubles (c1:c2:cs)
    | (c1:c2:"") `isInfixOf` cs = True
    | otherwise = hasDoubles (c2:cs)


isNice str = hasABA str && hasDoubles str

main = do
    input <- readFile "input.txt"
    let input_lines = lines input
    print $ length $ filter isNice input_lines
