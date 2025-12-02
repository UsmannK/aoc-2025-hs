main :: IO ()
main = do
    contents <- readFile "day1.txt"
    let rotations = parseInput contents
    let outputs = scanl (+) 50 rotations
    -- part 1
    print $ length $ filter (\x -> x `mod` 100 == 0) outputs

    -- part 2
    let countZeros :: Int -> Int -> Int
        countZeros a b = abs (a `quot` 100 - b `quot` 100)
    print $ sum $ zipWith countZeros outputs (tail outputs)

parseInput :: String -> [Int]
parseInput i = map parseLine (lines i)

parseLine :: String -> Int
parseLine ('L':n) = -read n
parseLine ('R':n) = read n
parseLine _ = error "error"