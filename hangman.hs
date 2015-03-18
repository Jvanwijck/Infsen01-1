import System.IO

hangman :: IO () 
hangman = do 
		putStrLn "Think of a word: " 
 		word <- sgetLine 
 		putStrLn "Try to guess it: " 
 		guess word 7

sgetLine :: IO String 
sgetLine = do 	
		x <- getChar
 		if x == '\n' then 
 			do 	
				putChar x 
 				return [] 
 		else 
 			do 
				putChar '-' 
 				xs <- sgetLine 
 				return (x:xs) 

guess :: String -> Int -> IO () 
guess word stop 
	| stop > 0 =	do 
				putStr "> " 
				xs <- getLine 
				if xs == word then 
 					putStrLn "You got it!" 
 				else 
 					do 
						putStrLn (diff word xs) 
 						guess word (stop - 1)
	| otherwise = putStrLn "Out of tries! You loose!" 

diff :: String -> String -> String 
diff xs ys = [if elem x ys then x else '-' | x <- xs] 