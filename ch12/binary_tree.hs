data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right -- element already there, don't change tree
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected then print "yup okay!" else error "test failed"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left v right) = v : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left v right) = inorder left ++ v : inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left v right) = postorder left ++ postorder right ++ [v]

testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f e = foldr f e . postorder



--- everything before this was copied from the last chapter
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
               Nothing -> Leaf
               Just (l,c,r) -> Node (unfold f l) c (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold op (0,n)
    where op (i,max)
            | i > max = Nothing
            | otherwise = Just ((i+1,n),i,(i+1,n))

