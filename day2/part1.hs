import System.IO
import Data.List.Split

parseDims :: String -> [Int]
parseDims str =map read $ splitOn "x" str

sideAreas :: Int -> Int -> Int -> [Int]
sideAreas l w h = [l * w, w * h, h * l]

wrappingRequired :: [Int] -> Int
wrappingRequired (x:y:z:[]) = minimum areas + 2 * sum areas
                        where areas = sideAreas x y z

main = do 
    contents <- readFile "input.txt"
    print $ sum $ map wrappingRequired $ map parseDims $ filter (/="") $ splitOn "\n" contents
