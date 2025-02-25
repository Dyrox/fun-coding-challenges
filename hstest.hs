
main :: IO ()
main = do
    input <- getLine            
    let [p1, p0, f, l] = map read (words input)
    putStrLn (solve (p1, p0, f, l))



toScore :: Int -> String
toScore n
    | n >= 9900000 = "EX+"
    | n >= 9800000 = "EX"
    | n >= 9500000 = "AA"
    | n >= 9200000 = "A"
    | n >= 8900000 = "B"
    | n >= 8600000 = "C"
    | otherwise = "D"

solve :: (Int,Int,Int,Int) -> String
solve (p1, p0, f, l) = toScore rawScore
  where 
    rawScore = (p1 + p0 + f `div` 2) * base + p1
    n = p1 + p0 + f + l 
    base = 10000000 `div` n
