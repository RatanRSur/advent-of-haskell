getFloor [] = 0
getFloor (x:xs) 
    | x == '(' = 1 + getFloor xs
    | x == ')' = -1 + getFloor xs

main = do
    input <- readFile "input.txt"
    putStrLn (show (getFloor input))
    return ()
