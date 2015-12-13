import Data.List

isVowel c = c `elem` "aeiou"

hasThreeVowels str = (length $ filter isVowel str) >= 3

hasNaughtySubs str
    | "ab" `isInfixOf` str = True
    | "cd" `isInfixOf` str = True
    | "pq" `isInfixOf` str = True
    | "xy" `isInfixOf` str = True
    | otherwise = False

hasDouble [] = False
hasDouble [c] = False
hasDouble (c1:c2:cs)
    | c1 == c2 = True
    | otherwise = hasDouble (c2:cs)

isNice str = hasThreeVowels str && hasDouble str && not (hasNaughtySubs str)

main = do
    input <- readFile "input.txt"
    let input_lines = lines input
    print $ length $ filter isNice input_lines
