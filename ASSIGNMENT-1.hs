isDigit :: Char -> Bool
isDigit x = contains x ['0','1','2','3','4','5','6','7','8','9']

contains :: Eq a => a -> [a] -> Bool
contains c [] = False
contains c (x:xs) = c == x || contains c xs

containsDigit :: String -> Bool 
containsDigit [] = False
containsDigit (x:xs) = isDigit x || (containsDigit xs)

digitCount :: String -> Int 
digitCount [] = 0
digitCount (x:xs) | isDigit x = 1 + digitCount xs
  		  | otherwise = digitCount xs 

digitIndex :: String -> Int 
digitIndex [] = -1
digitIndex (x:xs) | not (isDigit x) = if (containsDigit xs) then 1 + digitIndex xs else -1
		  | otherwise 	    = 0

evenCount :: [Int] -> Int 
evenCount xs = length [x | x <- xs, x `mod` 2 == 0]

containsEven :: [Int] -> Bool 
containsEven xs = evenCount xs > 0

allEven :: [Int] -> Bool 
allEven xs = [x | x <- xs, x `mod` 2 == 0] == xs

evenSum :: [Int] -> Int 
evenSum xs = sum [x | x <- xs, x `mod` 2 == 0]

isSquare :: Int -> Bool
isSquare n | n < 1     = False
	   | otherwise = contains n [x | x <- map (^2) [1..n]] 

containsSquare :: [Int] -> Bool 
containsSquare [] = False
containsSquare (x:xs) | isSquare x = True
		      | otherwise  = containsSquare xs

isOrdinal :: Ord(a) => [a] -> Bool 
isOrdinal []       = True
isOrdinal (x:y:xs) = x < y && isOrdinal (y:xs)
isOrdinal (x:xs)   = True

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = (factors n) == [1,n]

primeCount :: [Int] -> Int 
primeCount [] = 0
primeCount (x:xs) | isPrime x = 1 + primeCount xs
		  | otherwise = primeCount xs

containsDecimal :: String -> Bool 
containsDecimal xs = containsDigit xs

getDecimal :: String -> String
getDecimal [] = []
getDecimal (x:xs) | isDigit x = x : getDecimal xs
		  | otherwise = []

getDecimals :: String -> [Int]
getDecimals [] = []
getDecimals (x:xs) | isDigit x = (read (getDecimal (x:xs)) :: Int) : getDecimals (drop (length (getDecimal (x:xs))) (x:xs))
		   | otherwise = getDecimals xs

decimalCount :: String -> Int
decimalCount xs = length (getDecimals xs)

getBiggestInt :: [Int] -> Int
getBiggestInt (x:y:xs) | x > y     = getBiggestInt (x:xs)
		       | otherwise = getBiggestInt (y:xs)
getBiggestInt (x:xs) = x

longestDecimal :: String -> Int 
longestDecimal xs = length (show (maxDecimal xs))

maxDecimal :: String -> Int
maxDecimal xs = getBiggestInt (getDecimals xs)

intersection :: Eq(a) => [a] -> [a] -> [a]
intersection [] _ = []
intersection _ [] = []
intersection (x:xs) ys | contains x ys = x:(intersection [z | z <- xs, x /= z] ys)
		       | otherwise     = intersection xs ys

disjunct :: Eq(a) => [a] -> [a] -> [a]
disjunct [] _ = []
disjunct _ [] = []
disjunct (x:xs) ys | not (contains x ys) = x:(disjunct [z | z <- xs, x /= z] ys)
		   | otherwise 		 = disjunct xs ys

disjunction :: Eq(a) => [a] -> [a] -> [a]
disjunction [] _  = []
disjunction _ []  = []
disjunction xs ys = (disjunct xs ys) ++ (disjunct ys xs)

quickSort :: Ord(a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort [y | y <- xs, y <= x]) ++ [x] ++ (quickSort [z | z <- xs, z > x])

isPermutation :: Ord(a) => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _  = False
isPermutation _ []  = False
isPermutation xs ys = (quickSort xs) == (quickSort ys)

count :: Eq(a) => a -> [a] -> Int
count c [] = 0
count c (x:xs) | c == x    = 1 + (count c xs)
	       | otherwise = count c xs

equalCount :: String -> String -> String
equalCount [] [] = []
equalCount [] _  = []
equalCount _ []  = []
equalCount (x:xs) ys | (count x (x:xs)) == (count x ys) = x:(equalCount [z | z <- xs, x /= z] [y | y <- ys, x /= y])
		     | otherwise  		        = equalCount [z | z <- xs, x /= z] [y | y <- ys, x /= y]
