toDigitsRev    :: Integer -> [Integer]
toDigitsRev x
    | x > 0 = x `mod` 10 : toDigitsRev (x `div` 10)
    | otherwise = []


toRev    :: [Integer] -> [Integer]
toRev (x:xs) = toRev xs ++ [x]
toRev _ = []

toDigits    :: Integer -> [Integer]
toDigits x = toRev (toDigitsRev x)

main = do
    print("ex1-1", toDigits 1234 == [1,2,3,4])
    print("ex1-2", toDigitsRev 1234 == [4,3,2,1])
    print("ex1-3", toDigits 0 == [])
    print("ex1-4", toDigits (-17) == [])