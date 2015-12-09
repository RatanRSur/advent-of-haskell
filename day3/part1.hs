import qualified Data.Set as Set

generateSantaVisited "" = [(0,0)]
generateSantaVisited (c:cs) = newLoc c $ generateSantaVisited cs
    where
        newLoc c (x:xs)
            | c == '^' = (fst x, snd x + 1):x:xs
            | c == 'v' = (fst x, snd x - 1):x:xs
            | c == '<' = (fst x - 1, snd x):x:xs
            | c == '>' = (fst x + 1, snd x):x:xs
        

main = do 
        input <- readFile "input.txt"
        print $ length $ Set.fromList $ generateSantaVisited input
