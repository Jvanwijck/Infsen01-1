--			Felix van Leeuwen		--
--			Opdracht A			--
--							--

import System.IO

data Kleur = Rood | Blauw | Geel deriving (Show)

data Geofig = Vierkant Float Kleur | Rechthoek Float Float Kleur | Driehoek Float Kleur | Cirkel Float Kleur deriving (Show)

test :: IO()
test = do
		putStrLn (show (area (Vierkant 3 Geel)))
		putStrLn (show (area (Rechthoek 4 3 Blauw)))
		putStrLn (show (area (Driehoek 3 Rood)))
		putStrLn (show (area (Cirkel 3 Geel)))
		putStrLn (show (circumference (Vierkant 3 Geel)))
		putStrLn (show (circumference (Rechthoek 4 3 Blauw)))
		putStrLn (show (circumference (Driehoek 3 Rood)))
		putStrLn (show (circumference (Cirkel 3 Geel)))
		putStrLn (show (getPercentagesOfTotalArea (getShapesByKleur Geel [Vierkant 3 Geel, Vierkant 3 Blauw, Vierkant 3 Blauw, Rechthoek 3 3 Geel])))
		putStrLn (show (treeIsBalanced treeTrue))
		putStrLn (show (treeIsBalanced treeFalse))


area :: Geofig -> Float 
area (Vierkant x _) = x * x
area (Rechthoek x y _) = x * y
area (Driehoek x _) = ((sqrt (x^2 - (x / 2)^2)) * x) / 2
area (Cirkel x _) = pi * x^2

circumference :: Geofig -> Float
circumference (Vierkant x _) = x * 4
circumference (Rechthoek x y _) = x + x + y + y
circumference (Driehoek x _) = x * 3
circumference (Cirkel x _) = pi * x * 2

getVierkanten :: [Geofig] -> [Geofig]
getVierkanten xs = getShapes "Vierkant" xs

getRechthoeken :: [Geofig] -> [Geofig]
getRechthoeken xs = getShapes "Rechthoek" xs

getDriehoeken :: [Geofig] -> [Geofig]
getDriehoeken xs = getShapes "Driehoek" xs

getCirkels :: [Geofig] -> [Geofig]
getCirkels xs = getShapes "Cirkel" xs

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (y:ys) (x:xs)	| y == x 	= True && startsWith ys xs
				| otherwise	= False

contains :: Eq a => [a] -> [a] -> Bool
contains [] _ = True
contains _ [] = False
contains (y:ys) (x:xs)	| y == x	= startsWith ys xs
			| otherwise 	= contains (y:ys) xs

getShapes :: String -> [Geofig] -> [Geofig]
getShapes _ [] = []
getShapes ys (x:xs)	| contains ys (show x) 	= x:(getShapes ys xs)
			| otherwise 		= getShapes ys xs

getShapesByKleur :: Kleur -> [Geofig] -> [Geofig]
getShapesByKleur _ [] = []
getShapesByKleur ys (x:xs) 	| contains (show ys) (show x) 	= x:(getShapesByKleur ys xs)
				| otherwise			= getShapesByKleur ys xs

getLargestArea :: [Geofig] -> Maybe Geofig
getLargestArea [] = Nothing
getLargestArea (x:y:xs) | (area x) > (area y) 	= getLargestArea (x:xs)
			| otherwise		= getLargestArea (y:xs)
getLargestArea (x:xs) = Just x

getLargestCircumference :: [Geofig] -> Maybe Geofig
getLargestCircumference [] = Nothing
getLargestCircumference (x:y:xs)	| (circumference x) > (circumference y) = getLargestCircumference (x:xs)
					| otherwise 				= getLargestCircumference (y:xs)
getLargestCircumference (x:xs) = Just x

addGeofig :: [Geofig] -> Geofig -> [Geofig]
addGeofig xs x = (x:xs)

doMagic :: [Geofig] -> Float -> [Float]
doMagic [] f = []
doMagic (x:xs) f = (((area x) / f) * 100):(doMagic xs f)

getPercentagesOfTotalArea :: [Geofig] -> [Float]
getPercentagesOfTotalArea xs = doMagic xs (sum (map area xs))

--							--
--			Opdracht B			--
--							--

data Tree = Empty | Leaf Int | Node Tree Int Tree deriving (Show)

treeTrue :: Tree
treeTrue = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 8))

treeFalse :: Tree
treeFalse = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Leaf 6)

treeDepthLeft :: Tree -> Int
treeDepthLeft (Empty) = 0
treeDepthLeft (Leaf x) = 1
treeDepthLeft (Node left x right) = 1 + (treeDepthLeft left)

treeDepthRight :: Tree -> Int
treeDepthRight (Empty) = 0
treeDepthRight (Leaf x) = 1
treeDepthRight (Node left x right) = 1 + (treeDepthRight right)

treeIsBalanced :: Tree -> Bool
treeIsBalanced (Node left x right) = (treeIsBalanced left) && ((treeDepthLeft left) == (treeDepthRight right)) && (treeIsBalanced right)
treeIsBalanced x = True

--							--
--			Opdracht C			--
--			niet werkend gekregen				--

--type Parser a = String -> [(a,String)] 

--item :: Parser Char 
--item = \inp -> case inp of 
-- 		[] 	-> [] 
-- 		(x:xs) 	-> [(x,xs)] 

--failure :: Parser a 
--failure = \inp -> []

--return :: a -> Parser a 
--return v = \inp -> [(v,inp)] 

--(+++) :: Parser a -> Parser a -> Parser a 
--p +++ q = \inp -> case parse p inp of 
--			[] 		-> parse q inp 
--			[(v,out)] 	-> [(v,out)] 

--parse :: Parser a -> String -> [(a,String)] 
--parse p inp = p inp

--p :: Parser (Char,Char) 
--p = do
--	x <- item 
--	item 
--	y <- item 
--	Main.return (x,y)

--sat :: (Char -> Bool) -> Parser Char 
--sat p = do 
--		x <- item 
--		if p x then 
--			Main.return x 
--		else 
--			failure

--isDigit :: Char -> Bool
--isDigit x = [y | y <- ['0','1','2','3','4','5','6','7','8','9'], y == x] == [x]

--digit :: Parser Char 
--digit = sat isDigit 

--char :: Char -> Parser Char 
--char x = sat (x ==) 

--many :: Parser a -> Parser [a] 
--many p = many1 p +++ Main.return [] 

--many1 :: Parser a -> Parser [a] 
--many1 p = do 
--		v <- p 
--		vs <- many p 
--		Main.return (v:vs)

--string :: String -> Parser String 
--string [] = Main.return [] 
--string (x:xs) = do 
--			char x 
--			string xs 
--			Main.return (x:xs) 

--p :: Parser String 
--p = do 
--	char '[' 
--	d <- digit 
--	ds <- many (do 
--			char ',' 
--			digit) 
--	char ']' 
--	Main.return (d:ds) 

--expr -> term ('+' expr | e)
 
--term -> factor ('*' term | e)
 
--factor -> digit | '(' expr ')' 
 
--digit -> '0' | '1' | ... | '9'

--expr :: Parser Int 
--expr = do	
--		t <- term 
--		do	
--			char '+' 
--			e <- expr 
--			Main.return (t + e) +++ Main.return t
--		do	
--			char '-' 
--			e <- expr 
--			Main.return (t - e) +++ Main.return t

--term :: Parser Int 
--term = do 
--		f <- factor 
--		do 
--			char '*' 
--			t <- term 
--			Main.return (f * t) +++ Main.return f
--		do
--			char '/' 
--			t <- term 
--			Main.return (f / t) +++ Main.return f

--factor :: Parser Int 
--factor = do 
--		d <- digit 
--		Main.return d +++ (do 
--							char '(' 
--				 			e <- expr 
--							char ')' 
--							Main.return e)

--eval :: String -> Int 
--eval xs = fst (head (parse expr xs)) 
