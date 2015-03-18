import System.IO

stargame :: IO ()
stargame = move ["******","****","***","**","*"] 1 1 1

-- gameboard, playerno, rowno, removecount, row
move :: [[Char]] -> Int -> Int -> Int -> IO ()
move gbs playerno rowno removecount = do
					if (length (gbs !! (rowno - 1))) >= removecount then
						if 	(checkVictory (removeStar gbs rowno removecount)) then 
							do
								putStrLn ((show playerno) ++ " won!") 
								stargame
						else
							do
								showBoard (removeStar gbs rowno removecount)
								putStrLn ("It's player " ++ (show (playerno * (-1))) ++ "'s turn")
								a <- getInt "Row number: "
								b <- getInt "Remove: "
								move (removeStar gbs rowno removecount) (playerno * (-1)) a b
					else
						move (("*"++(head gbs)):(tail gbs)) (playerno * (-1)) 1 1


getInt :: String -> IO Int
getInt zs = do
		putStr zs
		xs <- getLine
		if (length xs > 0) && (isValidDigit (head xs)) then
			return (read [(head xs)] :: Int)
		else
			do
				y <- getInt zs
				return y

removeStar :: [[Char]] -> Int -> Int -> [[Char]]
removeStar [] _ _ = []
removeStar (x:xs) rowno removecount 
		| rowno == 1 	= ((drop removecount x):xs)
		| otherwise	= x:(removeStar xs (rowno - 1) removecount)

checkVictory :: [[Char]] -> Bool
checkVictory [] = True
checkVictory (x:xs) = (length x) == 0 && checkVictory xs

showBoard :: [[Char]] -> IO ()
showBoard [] = putStrLn ""
showBoard (x:xs) = do 
			putStrLn ((show (5 - (length xs))) ++ ": " ++ x)
			showBoard xs

isValidDigit :: Char -> Bool
isValidDigit x = contains x ['1','2','3','4','5']

contains :: Eq a => a -> [a] -> Bool
contains c [] = False
contains c (x:xs) = c == x || contains c xs