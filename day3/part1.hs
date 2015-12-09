import qualified Data.Set as Set

santaPos (x,y) (c:cs)
    | c == '^' = santaPos (x, y+1) cs
    | c == 'v' = santaPos (x, y-1) cs
    | c == '<' = santaPos (x-1, y) cs
    | c == '>' = santaPos (x+1, y) cs

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
