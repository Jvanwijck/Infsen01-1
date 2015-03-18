{- Scott Hoefnagel 0840620 -}
{- Jeffrey van Wijck 0837734 -}
{- INF 4V -}


{- Eerst stellen we vast wat we onder cijfers verstaan.  -}
isCijfer :: Char -> Bool
isCijfer x = bevat x ['0','1','2','3','4','5','6','7','8','9']

{- Deze functie vergelijkt, het kijkt of de opgegeven 
string een caracter bevat die in de array zit  -}
bevat :: Eq a => a -> [a] -> Bool
bevat  c [] = False
bevat c (x:xs) = c == x || bevat c xs

{- De functie combineert het bovenstaande, 
als er cijfer dat is vastgesteld in de eerste 
functie overeen komt met wat er in de array staat,
dan geeft deze een true terug.  -}
containsDigit :: String -> Bool
containsDigit [] = False
containsDigit (x:xs) = isCijfer x || (containsDigit xs)

{- De functie kijk of het caracter een cijfer is. 
Ja -> +1 bij het aantal cijfers
Nee -> dit getal blijft hetzelfde -}
digitCount :: String -> Int 
digitCount [] = 0
digitCount (x:xs) | isCijfer x = 1 + digitCount xs
  		  | otherwise = digitCount xs 
		  
{- In deze functie kijken we of het caracter een cijfer is. 
Ja ->  de index + 1
Nee-> de index = 1-}
digitIndex :: String -> Int 
digitIndex [] = 0
digitIndex (x:xs) | not (isCijfer x) = if (containsDigit xs) then 1 + digitIndex xs else 1
		  | otherwise = 1	

{- Als een getal een meervoud is van 2 dan betekent dit dat het een 
even getals is. Dit word gevonden door de mod (modulo).
Als een getal modulo 2 gelijk is aan 0 dan is het een even getal.  -}
evenCount :: [Int] -> Int 
evenCount xs = length [x | x <- xs, x `mod` 2 == 0]	

{- We kijken of een getal even is. is dit zo,
dan word er 1 bij opgeteld en is dit dus > 0 
er word een true terug gegeven. -}
containsEven :: [Int] -> Bool 
containsEven xs = evenCount xs > 0

{- Nu kijken we weer of elk getal een even getal is of niet,
is dit het geval dan krijg je true. 
Zo niet dan een false. -}
allEven :: [Int] -> Bool 
allEven xs = [x | x <- xs, x `mod` 2 == 0] == xs

{- Alle gevonden even getallen worden bij elkaar opgeteld. -}
evenSum :: [Int] -> Int 
evenSum xs = sum [x | x <- xs, x `mod` 2 == 0]

{- Een getal word bekeken of deze een kwadraat is van een ander getal. -}
isSquare :: Int -> Bool
isSquare n | n < 1     = False
	   | otherwise = bevat n [x | x <- map (^2) [1..n]] 

{- In de lijst word gekeken of er een kwadraat instaat. -}	   
containsSquare :: [Int] -> Bool 
containsSquare [] = False
containsSquare (x:xs) | isSquare x = True
		      | otherwise  = containsSquare xs

{- Er word gekeken of er een opvolgend getal is,
Ja --> er word gekeken of het volgende getal groter is als het huidige 
en is dit bij alle getallen zo, 
dan word er bij de laatste als nog een true teruggegeven. 
Alle andere gevallen geven een false. -}	  
isOrdinal :: Ord(a) => [a] -> Bool 
isOrdinal []       = True
isOrdinal (x:y:xs) = x < y && isOrdinal (y:xs)
isOrdinal (x:xs)   = True

{- Maakt een lijst met alle getallen n, waardoor getal x gedeeld kan worden zonder getal achter de komma. -}
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

{- Controleert of de lijst met getallen waardoor gedeeld kan worden gelijk is aan 1 en het getal zelf.
Ja -> het is een priemgetal
Nee -> het is geen priemgetal.-}
isPrime :: Int -> Bool
isPrime n = (factors n) == [1,n]

{- telt het aantal priemgetallen -}
primeCount :: [Int] -> Int 
primeCount [] = 0
primeCount (x:xs) | isPrime x = 1 + primeCount xs
		  | otherwise = primeCount xs

{- Er word gekeken of er in de string een decimaal getal voorkomt.  -}
containsDecimal :: String -> Bool 
containsDecimal xs = containsDigit xs

{- Er word per caracter gekeken of het een cijfer is.
Ja -> het hudige caracter wordt aan de string toegevoegd
Nee -> stop met de huidige string -}
getDecimal :: String -> String
getDecimal [] = []
getDecimal (x:xs) | isCijfer x = x : getDecimal xs
		  | otherwise = []

{- er word door de lijst gelopen, om te kijken welke decimalen er voor komen.
Indien er decimalen gevonden worden, komt er een lijstje terug met de getallen. -}
getDecimals :: String -> [Int]
getDecimals [] = []
getDecimals (x:xs) | isCijfer x = (read (getDecimal (x:xs)) :: Int) : getDecimals (drop (length (getDecimal (x:xs))) (x:xs))
		   | otherwise = getDecimals xs

{- het aantal getallen wat is gevonden met getDecimals wordt weergegeven. -}
decimalCount :: String -> Int
decimalCount xs = length (getDecimals xs)

{- Er word gekeken of het huidige getal groter is dan het volgende getal.
Ja -> het hudige getal is het nieuwe grootste getal
Nee -> het volgende getal wordt het grootste getal. -}
getBiggestInt :: [Int] -> Int
getBiggestInt (x:y:xs) | x > y     = getBiggestInt (x:xs)
		       | otherwise = getBiggestInt (y:xs)
getBiggestInt (x:xs) = x

{- de lengte van het grootste getal dat wordt gevonden met maxDecimal -}
longestDecimal :: String -> Int 
longestDecimal xs = length (show (maxDecimal xs))

{- Uit de string worden alle cijfers geplaatst in een array geplaatst
en uit die array wordt de grootste geselecteerd.-}
maxDecimal :: String -> Int
maxDecimal xs = getBiggestInt (getDecimals xs)

{- er word per caracter uit lijst A gekeken of die ook in lijst B voorkomt -}
intersection :: Eq(a) => [a] -> [a] -> [a]
intersection [] _ = []
intersection _ [] = []
intersection (x:xs) ys | bevat x ys = x:(intersection [z | z <- xs, x /= z] ys)
		       | otherwise     = intersection xs ys

{- er word per caracter uit lijst A gekeken of die juist niet in lijst B voorkomt -}
disjunct :: Eq(a) => [a] -> [a] -> [a]
disjunct [] _ = []
disjunct _ [] = []
disjunct (x:xs) ys | not (bevat x ys) = x:(disjunct [z | z <- xs, x /= z] ys)
		   | otherwise 		 = disjunct xs ys

{- scheid de caracters van elkaar, en voegt de resultaten uit lijst A en B samen -}
disjunction :: Eq(a) => [a] -> [a] -> [a]
disjunction [] _  = []
disjunction _ []  = []
disjunction xs ys = (disjunct xs ys) ++ (disjunct ys xs)

{- zet een lijst op volgorde van laagste waarde naar hoogste-}
quickSort :: Ord(a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort [y | y <- xs, y <= x]) ++ [x] ++ (quickSort [z | z <- xs, z > x])

{- Controleert of lijst A hetzelfde is als lijst B na het sorteren.-}
isPermutation :: Ord(a) => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _  = False
isPermutation _ []  = False
isPermutation xs ys = (quickSort xs) == (quickSort ys)

{- Kijkt hoe vaak een element in de lijst voorkomt. -}
tel :: Eq(a) => a -> [a] -> Int
tel c [] = 0
tel c (x:xs) | c == x    = 1 + (tel c xs)
	       | otherwise = tel c xs

{- Kijkt of elk element in lijst a evenveel voorkomt als in lijst b.
Ja -> voeg het element toe aan de string
Nee -> ga naar volgend element. -}
equalCount :: String -> String -> String
equalCount [] [] = []
equalCount [] _  = []
equalCount _ []  = []
equalCount (x:xs) ys | (tel x (x:xs)) == (tel x ys) = x:(equalCount [z | z <- xs, x /= z] [y | y <- ys, x /= y])
		     | otherwise  		        = equalCount [z | z <- xs, x /= z] [y | y <- ys, x /= y]