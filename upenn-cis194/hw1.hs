-- EX1

toDigitsRev    :: Integer -> [Integer]
toDigitsRev x
    | x > 0 = x `mod` 10 : toDigitsRev (x `div` 10)
    | otherwise = []


toRev    :: [Integer] -> [Integer]
toRev (x:xs) = toRev xs ++ [x]
toRev _ = []

toDigits    :: Integer -> [Integer]
toDigits x = toRev (toDigitsRev x)


-- EX2

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev ([]) = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:y:xs) = [x, y*2] ++ doubleEveryOtherRev xs


doubleEveryOther x = toRev(doubleEveryOtherRev (toRev x))


-- EX3

sumDigit :: Integer -> Integer
sumDigit x 
    | x `div` 10 == 0 = x
    | otherwise = (x `mod` 10) + (sumDigit (x `div` 10))


sumDigits :: [Integer] -> Integer
sumDigits (x:xs) = sumDigit x + sumDigits xs
sumDigits [] = 0


-- EX4

validate :: Integer -> Bool
validate x = sumDigits(doubleEveryOther(toDigits(x))) `mod` 10 == 0


-- EX5


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1  a b c = [(a,b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ (hanoi (n-1)  c b a)




-- MAIN

main = do
    print("ex1-1", toDigits 1234 == [1,2,3,4])
    print("ex1-2", toDigitsRev 1234 == [4,3,2,1])
    print("ex1-3", toDigits 0 == [])
    print("ex1-4", toDigits (-17) == [])
    print("ex2-1", doubleEveryOther [8,7,6,5] == [16,7,12,5])
    print("ex2-2", doubleEveryOther [1,2,3] == [1,4,3])
    print("ex3-1", sumDigits [16,7,12,5] == 22)
    print("ex4-1", validate 4012888888881881 == True)
    print("ex4-2", validate 4012888888881882 == False)
    print("ex5", hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")])
    print("ex5-2", hanoi 3 "a" "b" "c" )



