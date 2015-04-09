----INFSEN01-1, ASSIGNMENT-2, 0837734 Jeffrey van Wijck, 0840620 Scott Hoefnagel

--------------Opdracht B--------------
data Tree = Leaf Int | Node Tree Int Tree | Empty deriving (Show)

---Maak 2 bomen aan, die aangeroepen kunnen worden met commandline.
treeTrue :: Tree
treeTrue = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 8))

treeFalse :: Tree
treeFalse = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Leaf 6)

---recursief kijken wat de diepte is van de linkerkant van de boom
treeDepthLeft :: Tree -> Int
treeDepthLeft (Empty) = 0
treeDepthLeft (Leaf x) = 1
treeDepthLeft (Node left x right) = 1 + (treeDepthLeft left)

---recursief kijken wat de diepte is van de rechterkant van de boom
treeDepthRight :: Tree -> Int
treeDepthRight (Empty) = 0
treeDepthRight (Leaf x) = 1
treeDepthRight (Node left x right) = 1 + (treeDepthRight right)

---Recursive loop waarbij er gecontroleerd wordt of de linkerkant steeds 2 leafs heeft, hetzelfde voor de rechterkant. Daarnaast wordt er ook recursief gekeken of de diepte van de linkerkant gelijk is aan de diepte van de rechterkant.
treeIsBalanced :: Tree -> Bool
treeIsBalanced (Node left x right) = (treeIsBalanced left) && ((treeDepthLeft left) == (treeDepthRight right)) && (treeIsBalanced right)
treeIsBalanced x = True


