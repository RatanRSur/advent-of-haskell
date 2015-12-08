parenToNum c = if c == '(' then 1 else -1
parenStrToNums = map parenToNum

basementPos _ (-1) pos = pos
basementPos (x:xs) sum pos = basementPos xs (sum + x) (pos + 1)

main = do
    input <- readFile "input.txt"
    let oneszeros = parenStrToNums input
    putStrLn (show (basementPos oneszeros 0 0))
