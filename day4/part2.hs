import qualified Data.Hash.MD5 as MD5

md5snum str x = MD5.md5s $ MD5.Str $ str ++ show x

firstSixZeros str x 
    | take 6 (md5snum str x) == "000000" = x
    | otherwise = -1

main = do
    let word_part = "iwrupvqb"
    print $ head [firstSixZeros word_part x | x <- [0..], firstSixZeros word_part x > 0]
