import System.IO
import Data.List.Split

parseDims :: String -> [Int]
parseDims str = map read $ splitOn "x" str

sideAreas :: Int -> Int -> Int -> [Int]
sideAreas l w h = [l * w, w * h, h * l]

sidePerims l w h = [2 * x | x <- [l + w, w + h, h + l]]

ribbonRequired (x:y:z:[]) = minimum (sidePerims x y z) + vol x y z

vol l w h = l * w * h


main = do 
    contents <- readFile "input.txt"
    print $ sum $ map ribbonRequired $ map parseDims $ filter (/="") $ splitOn "\n" contents
