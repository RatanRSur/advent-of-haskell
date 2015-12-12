import qualified Data.Hash.MD5 as MD5

md5snum str x = MD5.md5s $ MD5.Str $ str ++ show x

firstFiveZeros str x 
    | take 5 (md5snum str x) == "00000" = x
    | otherwise = -1

main = do
    let word_part = "iwrupvqb"
    print $ head [firstFiveZeros word_part x | x <- [0..], firstFiveZeros word_part x > 0]
