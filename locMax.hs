locMax :: [Int] -> [Int]
locMax[] = [0]
locMax (x:y:xs) = third $ foldl f (x,y,[]) xs
  where
    third (_, _, x) = x
    f (x, y, ls) z = (y, z, if y > x && y > z then y:ls else ls)