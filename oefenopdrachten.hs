n :: Int
n = a `div` (length xs)
 where 
  a = 10 
  xs = [1,2,3,4,5] 

double :: Int->Int
double x = x + x

quadruple :: Int->Int
quadruple x = double (double x)

factorial :: Int->Int
factorial x = product [1..x]

average :: [Int]->Int
average xs = div (sum xs) (length xs)

last1 :: [a]->a
last1 xs = head (reverse xs)

last2 :: [a]->a
last2 xs = xs !! ((length xs) - 1)

init1 :: [a]->[a]
init1 xs = reverse (tail $ reverse xs)

init2 :: [a]->[a]
init2 xs = take (length xs - 1) xs

add' :: Int -> (Int -> Int)
add' x y = x + y

--[Char]
--(Char,Char,Char)
--[(Bool,Char)]
--([Bool],[Char])
--[[a]->[a]]

binomiaal :: Int -> Int -> Int
binomiaal n k = factorial n  `div` factorial k * factorial (n - k)

--[a] -> a
--(a,b) -> (b,a)
--a->b->(a,b)
--Num(a) a->a
--[a]->bool
--(a->b)->a->b

safetaila :: [a] -> [a]
safetaila xs = if (length xs > 0) then (tail xs) else []

safetailb :: [a] -> [a]
safetailb xs | length xs > 0 = tail xs
             | otherwise     = []


safetailc :: [a] -> [a]
safetailc [] = []
safetailc xs = tail xs

--(||) :: Bool -> Bool -> Bool
--False || False = False
--_ || _ = True

--(&&) :: Bool -> Bool -> Bool
--a && b = if a == True then if b == True then True else False else False

--(&&) :: Bool -> Bool -> Bool
--a && b = if a == True then b else False

pyth :: Int -> [(Int, Int, Int)]
pyth n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

en :: [Bool] -> Bool
en [] = True
en (x:xs) = x && (en xs)

concateneer :: [[a]] -> [a] 
concateneer [] = []
concateneer (x:xs) = x ++ (concateneer xs)

repliceer :: Int -> a -> [a]
repliceer 0 a = []
repliceer n a = a:(repliceer (n-1) a)

(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

element :: Eq a => a -> [a] -> Bool 
element c [] = False
element c (x:xs) = c == x || element c xs

voegsamen :: [Int] -> [Int] -> [Int] 
voegsamen [] ys = ys
voegsamen xs [] = xs
voegsamen (x:xs) (y:ys) | x < y = x:(voegsamen xs (y:ys))
			| otherwise = y:(voegsamen (x:xs) ys)

mengsort :: [Int] -> [Int] 
mengsort [] = []
mengsort [x] = [x]
mengsort (x:xs) = mengsort [y | y <- xs, y <= x]) ++ [x] ++ (mengsort [z | z <- xs, z > x])

opdracht4 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
opdracht4 f p xs = map f (filter p xs)

opdracht5map :: (a -> b) -> [a] -> [b]
opdracht5map f xs = foldr (\y ys -> [(f y)] ++ ys) [] xs

opdracht5filter :: (a -> Bool) -> [a] -> [a]
opdracht5filter p xs = foldr (\y ys -> if (p y) then [y] ++ ys else ys) [] xs